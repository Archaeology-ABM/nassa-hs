{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module NASSA.ReadYml where

import           NASSA.BibTeX
import           NASSA.Types
import           NASSA.Utils

import           Control.Exception  (throwIO, try)
import           Control.Monad      (filterM, forM_, unless)
import qualified Data.ByteString    as B
import           Data.Char          (isSpace)
import           Data.Either        (lefts, rights)
import           Data.List          (elemIndex, intercalate, nub, (\\))
import           Data.Maybe         (maybeToList)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import           Data.Yaml          (decodeEither')
import           System.Directory   (doesDirectoryExist, doesFileExist,
                                     listDirectory)
import           System.FilePath    (takeDirectory, takeFileName, (</>))
import           System.IO          (IOMode (ReadMode), hGetContents, hPutStrLn,
                                     stderr, withFile)

readNassaModuleCollection :: FilePath -> IO [NassaModule]
readNassaModuleCollection baseDir = do
    -- search yml files
    hPutStrLn stderr "Searching NASSA.yml files... "
    yamlFilePaths <- findAllNassaYamlFiles baseDir
    hPutStrLn stderr $ show (length yamlFilePaths) ++ " found"
    -- remove yml files with wrong nassaVersion
    hPutStrLn stderr "Checking NASSA versions... "
    yamlFilePathsInVersionRange <- filterByNassaVersion yamlFilePaths
    -- parse yml files
    hPutStrLn stderr "Loading NASSA.yml files... "
    eitherYamls <- mapM (try . readNassaYaml) yamlFilePathsInVersionRange :: IO [Either NassaException NassaModule]
    unless (null . lefts $ eitherYamls) $ do
        hPutStrLn stderr "Some files were skipped:"
        forM_ eitherYamls $ \case
            Left e -> hPutStrLn stderr (renderNassaException e)
            _      -> return ()
    -- integrity checks
    let loadedYamlFiles = rights eitherYamls
    eitherModules <- mapM (try . checkIntegrity) loadedYamlFiles :: IO [Either NassaException NassaModule]
    unless (null . lefts $ eitherModules) $ do
        hPutStrLn stderr "Some modules are broken:"
        forM_ eitherModules $ \case
            Left e -> hPutStrLn stderr (renderNassaException e)
            _      -> return ()
    -- report success
    let goodModules = rights eitherModules
    hPutStrLn stderr "***"
    hPutStrLn stderr $ (show . length $ goodModules) ++ " loaded"
    return goodModules

findAllNassaYamlFiles :: FilePath -> IO [FilePath]
findAllNassaYamlFiles baseDir = do
    entries <- listDirectory baseDir
    let curFiles = map (baseDir </>) $ filter (=="NASSA.yml") $ map takeFileName entries
    subDirs <- filterM doesDirectoryExist . map (baseDir </>) $ entries
    moreFiles <- fmap concat . mapM findAllNassaYamlFiles $ subDirs
    return $ curFiles ++ moreFiles

filterByNassaVersion :: [FilePath] -> IO [FilePath]
filterByNassaVersion nassaYmlFiles = do
    eitherPaths <- mapM isInVersionRange nassaYmlFiles
    mapM_ (hPutStrLn stderr . renderNassaException) $ lefts eitherPaths
    return $ rights eitherPaths
    where
        isInVersionRange :: FilePath -> IO (Either NassaException FilePath)
        isInVersionRange ymlFile = do
            content <- readFile' ymlFile
            let ymlLines = lines content
            -- This implementation only works with a true YAML file.
            -- But technically also JSON is YAML. If somebody prepares
            -- a NASSA.yml file in JSON format, a wrong version
            -- can not be caught.
            case elemIndex "nassaVersion:" (map (take 13) ymlLines) of
                Nothing -> return $ Left $ NassaModuleMissingVersionException ymlFile
                Just n -> do
                    let versionLine = ymlLines !! n
                        versionString = filter (not . isSpace) $ drop 13 versionLine
                    if versionString `elem` map showNassaVersion validNassaVersions
                    then return $ Right ymlFile
                    else return $ Left $ NassaModuleVersionException ymlFile versionString
        readFile' :: FilePath -> IO String
        readFile' filename = withFile filename ReadMode $ \handle -> do
            theContent <- hGetContents handle
            mapM return theContent

readNassaYaml :: FilePath -> IO NassaModule
readNassaYaml yamlPath = do
    yamlRaw <- B.readFile yamlPath
    case decodeEither' yamlRaw of
        Left err  -> throwIO $ NassaYamlParseException yamlPath err
        Right pac -> return $ NassaModule (takeDirectory yamlPath, pac)

checkIntegrity :: NassaModule -> IO NassaModule
checkIntegrity (NassaModule (baseDir, yamlStruct)) = do
    checkFile "README.md"
    checkFile "CHANGELOG.md"
    checkFile "LICENSE"
    checkDocsDir
    checkCodeDirsExistence
    checkReferences
    return (NassaModule (baseDir, yamlStruct))
    where
        nassaID = _nassaYamlID yamlStruct
        checkFile :: FilePath -> IO ()
        checkFile p = do
            let path = baseDir </> p
            fe <- doesFileExist path
            unless fe $ throwIO $
                NassaModuleIntegrityException nassaID $
                show path ++ " does not exist"
        checkReadme :: IO ()
        checkReadme = do
            readmeFull <- fmap T.decodeUtf8Lenient $ B.readFile $ baseDir </> "README.md"
            let furtherInfo = T.concat $ dropWhile (/= "## Further information") $ T.lines readmeFull
                nrChars = T.length furtherInfo
            hPutStrLn stderr $ show nrChars
        checkDocsDir :: IO ()
        checkDocsDir = case _nassaYamlDocsDir yamlStruct of
            Nothing -> return ()
            Just p -> do
                let path = baseDir </> p
                fe <- doesDirectoryExist path
                unless fe $ throwIO $
                    NassaModuleIntegrityException nassaID $
                    "Documentation directory " ++ show path ++ " does not exist"
        checkCodeDirsExistence :: IO ()
        checkCodeDirsExistence = do
            let codeDirs = map (\x -> baseDir </> langInPathName (_implementationLanguage x) ++ "_implementation") $ _nassaYamlImplementations yamlStruct
            codeDirsExist <- mapM doesDirectoryExist codeDirs
            unless (and codeDirsExist) $ throwIO $
                NassaModuleIntegrityException nassaID $
                "One of the codeDirs (" ++ intercalate ", " codeDirs ++ ") does not exist"
        checkReferences :: IO ()
        checkReferences = do
            checkFile "references.bib"
            bib <- readBibTeXFile $ baseDir </> "references.bib"
            case _nassaYamlReferences yamlStruct of
                Nothing -> return ()
                Just (ReferenceStruct xs ys) -> do
                    let literatureInYml = nub $ concat $ maybeToList $ (++) <$> xs <*> ys
                    let literatureInBib = map bibEntryId bib
                    let literatureNotInBibButInYml = literatureInYml \\ literatureInBib
                    unless (null literatureNotInBibButInYml) $
                        throwIO $ NassaModuleIntegrityException nassaID $
                            "Some papers referenced in the NASSA.yml file (" ++
                            intercalate ", " literatureNotInBibButInYml ++ ") lack BibTeX entries"
