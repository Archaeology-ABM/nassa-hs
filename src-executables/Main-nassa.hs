{-# LANGUAGE OverloadedStrings #-}

import           Paths_nassa                 (version)

import           Control.Applicative        ((<|>))
import           Control.Exception          (Exception, throwIO, catch, try)
import           Control.Monad              (filterM, unless, forM_)
import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             (.:), (.:?), (.=), withText)
import qualified Data.ByteString            as B
import           Data.Either                (lefts, rights)
import           Data.Version               (showVersion)
import           Data.List                  (transpose, intercalate, sortOn)
import           Data.Yaml                  (decodeEither', ParseException)
import qualified Options.Applicative        as OP
import           System.Directory           (doesDirectoryExist,
                                             listDirectory)
import           System.Exit                (exitFailure)
import           System.FilePath            ((</>), takeFileName)
import           System.IO                  (hPutStrLn, stderr)
import           Text.Layout.Table          (asciiRoundS, column, def, expand,
                                             expandUntil, rowsG, tableString,
                                             titlesH)

-- exceptions

data NassaException =
    NassaYamlParseException FilePath ParseException -- ^ An exception to represent YAML parsing errors
    deriving Show

instance Exception NassaException

renderNassaException :: NassaException -> String
renderNassaException (NassaYamlParseException fn e) =
    "Could not parse YAML file " ++ fn ++ ": " ++ show e

-- data types

data ListOptions = ListOptions { 
      _inPath :: FilePath
    , _optRaw :: Bool
    }

data Options = CmdList ListOptions

data NassaInteractionsStruct = NassaInteractionsStruct {
      _nassaInteractionsDependencies :: Maybe [Int],
      _nassaInteractionsSuggests :: Maybe [Int]
    } deriving (Show, Eq)

instance FromJSON NassaInteractionsStruct where
    parseJSON = withObject "NassaInteractionsStruct" $ \v -> NassaInteractionsStruct
        <$> v .:   "dependencies"
        <*> v .:   "suggests"

data NassaLanguageStruct = 
      LanguageR 
    | LanguagePython
    | LanguageNetlogo
    deriving (Eq)

instance Show NassaLanguageStruct where
    show LanguageR = "R"
    show LanguagePython = "Python"
    show LanguageNetlogo = "Netlogo"

instance FromJSON NassaLanguageStruct where
    parseJSON = withText "language" $ \v -> case v of
        "R"         -> pure LanguageR
        "Python"    -> pure LanguagePython
        "Netlogo"   -> pure LanguageNetlogo
        _           -> fail ("unknown Language")

data NassaYamlStruct = NassaYamlStruct {
      _nassaYamlID :: Integer
    , _nassaYamlTitle :: String
    , _nassaYamlCategory :: String
    , _nassaYamlTags :: Maybe [String]
    , _nassaYamlAuthorship :: String
    , _nassaYamlLanguage :: NassaLanguageStruct
    , _nassaYamlLicense :: Maybe String
    , _nassaYamlInteractions :: Maybe NassaInteractionsStruct
    } deriving (Show, Eq)

instance FromJSON NassaYamlStruct where
    parseJSON = withObject "NassaYamlStruct" $ \v -> NassaYamlStruct
        <$> v .:   "id"
        <*> v .:   "title"
        <*> v .:   "category"
        <*> v .:?  "tags"
        <*> v .:   "authorship"
        <*> v .:   "language"
        <*> v .:?  "license"
        <*> v .:?  "interactions"

-- command line interface

main :: IO ()
main = do
    cmdOpts <- OP.customExecParser p optParserInfo
    catch (runCmd cmdOpts) handler
    where
        p = OP.prefs OP.showHelpOnEmpty
        handler :: NassaException -> IO ()
        handler e = do
            hPutStrLn stderr $ renderNassaException e
            exitFailure

runCmd :: Options -> IO ()
runCmd o = case o of
    CmdList opts -> runList opts

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (OP.helper <*> versionOption <*> optParser) (
    OP.briefDesc <>
    OP.progDesc "nassa"
    )

versionOption :: OP.Parser (a -> a)
versionOption = OP.infoOption (showVersion version) (OP.long "version" <> OP.help "Show version")

optParser :: OP.Parser Options
optParser = OP.subparser (
        OP.command "list" listOptInfo <>
        OP.commandGroup "Inspection commands:"
    )
  where
    listOptInfo = OP.info (OP.helper <*> (CmdList <$> listOptParser))
        (OP.progDesc "list")

listOptParser :: OP.Parser ListOptions
listOptParser = ListOptions <$> parseFilePath 
                            <*> parseRawOutput

parseFilePath :: OP.Parser FilePath
parseFilePath = OP.strOption (
    OP.long "baseDir" <>
    OP.short 'd' <>
    OP.help "root directory where to search for NASSA modules"
    )

parseRawOutput :: OP.Parser Bool
parseRawOutput = OP.switch (
    OP.long "raw" <> 
    OP.help "output table as tsv without header. Useful for piping into grep or awk"
    )

-- program logic

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
        forM_ (zip yamlFilePaths eitherYamls) $ \(posF, epac) -> do
            case epac of
                Left e -> do
                    hPutStrLn stderr (renderNassaException e)
                _ -> return ()
    let loadedYamlFiles = rights eitherYamls
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
    let tableH = ["id", "title", "category", "language"]
        tableB = transpose [
            map (show . _nassaYamlID) modules, 
            map _nassaYamlTitle modules, 
            map _nassaYamlCategory modules,
            map (show . _nassaYamlLanguage) modules
            ]
    if rawOutput
    then putStrLn $ intercalate "\n" [intercalate "\t" row | row <- tableB]
    else do
        let colSpecs = replicate (length tableH) (column (expandUntil 60) def def def)
        putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]