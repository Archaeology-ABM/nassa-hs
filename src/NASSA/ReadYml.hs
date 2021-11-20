{-# LANGUAGE LambdaCase #-}
module NASSA.ReadYml where

import           NASSA.Types
import           NASSA.Utils

import           Control.Exception          (throwIO, try)
import           Control.Monad              (filterM, unless, forM_)
import qualified Data.ByteString            as B
import           Data.Either                (lefts, rights)
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
    checkExistence doesFileExist _nassaYamlReadmeFile "readmeFile"
    checkExistence doesDirectoryExist _nassaYamlDocsDir "docsDir"
    checkExistence doesFileExist _nassaYamlDesignDetailsFile "designDetailsFile"
    return (NassaModule (baseDir, yamlStruct))
    where
        nassaID = _nassaYamlID yamlStruct
        checkExistence f el elS = 
            case el yamlStruct of
                Nothing -> return ()
                Just p -> do 
                    fe <- f $ baseDir </> p
                    unless fe $ throwIO (
                        NassaModuleIntegrityException nassaID $
                        elS ++ " " ++ show p ++ " does not exist"
                        )
