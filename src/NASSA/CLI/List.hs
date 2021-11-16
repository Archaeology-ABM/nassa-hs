module NASSA.CLI.List where

import           NASSA.Types
import           NASSA.Utils

import           Control.Exception          (throwIO, try)
import           Control.Monad              (filterM, unless, forM_)
import qualified Data.ByteString            as B
import           Data.Either                (lefts, rights)
import           Data.List                  (transpose, intercalate)
import           Data.Yaml                  (decodeEither')
import           System.Directory           (doesDirectoryExist,
                                             listDirectory)
import           System.FilePath            ((</>), takeFileName)
import           System.IO                  (hPutStrLn, stderr)
import           Text.Layout.Table          (asciiRoundS, column, def,
                                             expandUntil, rowsG, tableString,
                                             titlesH)


data ListOptions = ListOptions { 
      _inPath :: FilePath
    , _optRaw :: Bool
    }

runList :: ListOptions -> IO ()
runList (ListOptions baseDir rawOutput) = do
    yamlCollection <- readNassaModuleCollection baseDir
    printModuleTable rawOutput yamlCollection

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

printModuleTable :: Bool -> [NassaYamlStruct] -> IO ()
printModuleTable rawOutput modules = do
    let tableH = ["id", "title", "first author", "languages"]
        tableB = transpose [
              map _nassaYamlID modules
            , map _nassaYamlTitle modules
            , map (_contributorName . head . _nassaYamlContributors) modules
            , map (show . _nassaYamlProgrammingLanguage) modules
            ]
    if rawOutput
    then putStrLn $ intercalate "\n" [intercalate "\t" row | row <- tableB]
    else do
        let colSpecs = replicate (length tableH) (column (expandUntil 60) def def def)
        putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]