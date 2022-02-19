{-# LANGUAGE LambdaCase #-}
module NASSA.ReadYml where

import           NASSA.BibTeX
import           NASSA.Types
import           NASSA.Utils

import           Control.Exception          (throwIO, try)
import           Control.Monad              (filterM, unless, forM_)
import qualified Data.ByteString            as B
import           Data.Either                (lefts, rights)
import           Data.List                  (intercalate, nub, (\\))
import           Data.Maybe                 (maybeToList)
import           Data.Yaml                  (decodeEither')
import           System.Directory           (doesDirectoryExist, doesFileExist, 
                                             listDirectory)
import           System.FilePath            ((</>), takeFileName, takeDirectory)
import           System.IO                  (hPutStrLn, stderr)

readNassaModuleCollection :: FilePath -> IO [NassaModule]
readNassaModuleCollection baseDir = do
    -- search yml files
    hPutStrLn stderr "Searching NASSA.yml files... "
    yamlFilePaths <- findAllNassaYamlFiles baseDir
    hPutStrLn stderr $ show (length yamlFilePaths) ++ " found"
    -- parse yml files
    hPutStrLn stderr "Loading NASSA.yml files... "
    eitherYamls <- mapM (try . readNassaYaml) yamlFilePaths :: IO [Either NassaException NassaModule]
    unless (null . lefts $ eitherYamls) $ do
        hPutStrLn stderr "Some files were skipped:"
        forM_ eitherYamls $ \case
            Left e -> hPutStrLn stderr (renderNassaException e)
            _ -> return ()
    -- integrity checks
    let loadedYamlFiles = rights eitherYamls
    eitherModules <- mapM (try . checkIntegrity) loadedYamlFiles :: IO [Either NassaException NassaModule]
    unless (null . lefts $ eitherModules) $ do
        hPutStrLn stderr "Some modules are broken:"
        forM_ eitherModules $ \case
            Left e -> hPutStrLn stderr (renderNassaException e)
            _ -> return ()
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

readNassaYaml :: FilePath -> IO NassaModule
readNassaYaml yamlPath = do
    yamlRaw <- B.readFile yamlPath
    case decodeEither' yamlRaw of
        Left err  -> throwIO $ NassaYamlParseException yamlPath err
        Right pac -> return $ NassaModule (takeDirectory yamlPath, pac)

checkIntegrity :: NassaModule -> IO NassaModule
checkIntegrity (NassaModule (baseDir, yamlStruct)) = do
    -- file existence checks
    checkExistence doesFileExist _nassaYamlReadmeFile "readmeFile"
    checkExistence doesDirectoryExist _nassaYamlDocsDir "docsDir"
    checkExistence doesFileExist _nassaYamlDesignDetailsFile "designDetailsFile"
    checkCodeDirsExistence
    checkExistence doesFileExist (fmap _referencesBibFile . _nassaYamlReferences) "bibFile"
    -- reference/bibtex integrity
    checkReferences (_nassaYamlReferences yamlStruct)
    -- return package
    return (NassaModule (baseDir, yamlStruct))
    where
        nassaID = _nassaYamlID yamlStruct
        checkExistence :: (FilePath -> IO Bool) -> (NassaModuleYamlStruct -> Maybe FilePath) -> [Char] -> IO ()
        checkExistence f el elS = 
            case el yamlStruct of
                Nothing -> return ()
                Just p -> do 
                    fe <- f $ baseDir </> p
                    unless fe $ throwIO $
                        NassaModuleIntegrityException nassaID $
                        elS ++ " " ++ show p ++ " does not exist"
        checkCodeDirsExistence :: IO ()
        checkCodeDirsExistence = do
            let codeDirs = map _implementationCodeDir $ _nassaYamlImplementations yamlStruct
            codeDirsExist <- mapM (\x -> doesDirectoryExist $ baseDir </> x) codeDirs
            unless (and codeDirsExist) $ throwIO $
                NassaModuleIntegrityException nassaID $
                "One of the codeDirs (" ++ intercalate ", " codeDirs ++ ") does not exist"
        checkReferences :: Maybe ReferenceStruct -> IO ()
        checkReferences Nothing = return ()
        checkReferences (Just (ReferenceStruct bibFilePath xs ys)) = do
            -- read bibtex file
            bib <- readBibTeXFile $ baseDir </> bibFilePath
            -- match keys
            let literatureInYml = nub $ concat $ maybeToList $ (++) <$> xs <*> ys
            let literatureInBib = map bibEntryId bib
            let literatureNotInBibButInYml = literatureInYml \\ literatureInBib
            unless (null literatureNotInBibButInYml) $ 
                throwIO $ NassaModuleIntegrityException nassaID $
                    "Some papers referenced in the NASSA.yml file (" ++
                    intercalate ", " literatureNotInBibButInYml ++ ") lack BibTeX entries"
