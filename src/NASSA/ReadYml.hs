module NASSA.ReadYml where

import           NASSA.Types
import           NASSA.Utils

import           Control.Exception          (throwIO, try)
import           Control.Monad              (filterM, unless, forM_)
import qualified Data.ByteString            as B
import           Data.Either                (lefts, rights)
import           Data.Yaml                  (decodeEither')
import           System.Directory           (doesDirectoryExist, listDirectory)
import           System.FilePath            ((</>), takeFileName)
import           System.IO                  (hPutStrLn, stderr)

readNassaModuleCollection :: FilePath -> IO [NassaYamlStruct]
readNassaModuleCollection baseDir = do
    hPutStrLn stderr "Searching NASSA.yml files... "
    yamlFilePaths <- findAllNassaYamlFiles baseDir
    hPutStrLn stderr $ show (length yamlFilePaths) ++ " found"
    hPutStrLn stderr "Loading NASSA modules... "
    eitherYamls <- mapM (try . readNassaYaml) yamlFilePaths :: IO [Either NassaException NassaYamlStruct]
    unless (null . lefts $ eitherYamls) $ do
        hPutStrLn stderr "Some modules were skipped:"
        forM_ (zip yamlFilePaths eitherYamls) $ \(_, epac) -> do
            case epac of
                Left e -> do
                    hPutStrLn stderr (renderNassaException e)
                _ -> return ()
    let loadedYamlFiles = rights eitherYamls
    hPutStrLn stderr "***"
    hPutStrLn stderr $ (show . length $ loadedYamlFiles) ++ " loaded"
    return loadedYamlFiles

findAllNassaYamlFiles :: FilePath -> IO [FilePath]
findAllNassaYamlFiles baseDir = do
    entries <- listDirectory baseDir
    let curFiles = map (baseDir </>) $ filter (=="NASSA.yml") $ map takeFileName entries
    subDirs <- filterM doesDirectoryExist . map (baseDir </>) $ entries
    moreFiles <- fmap concat . mapM findAllNassaYamlFiles $ subDirs
    return $ curFiles ++ moreFiles

readNassaYaml :: FilePath -> IO NassaYamlStruct
readNassaYaml yamlPath = do
    yamlRaw <- B.readFile yamlPath
    case decodeEither' yamlRaw of
        Left err  -> throwIO $ NassaYamlParseException yamlPath err
        Right pac -> return pac