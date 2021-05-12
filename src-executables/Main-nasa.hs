{-# LANGUAGE OverloadedStrings #-}

import           Paths_nasa                 (version)

import           Control.Applicative        ((<|>))
import           Control.Exception          (Exception, throwIO, catch, try)
import           Control.Monad              (filterM, unless, forM_)
import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             (.:), (.:?), (.=))
import qualified Data.ByteString            as B
import           Data.Version               (showVersion)
import           Data.Yaml                  (decodeEither', ParseException)
import qualified Options.Applicative        as OP
import           System.Directory           (doesDirectoryExist,
                                             listDirectory)
import           System.Exit                (exitFailure)
import           System.FilePath            ((</>), takeFileName)
import           System.IO                  (hPutStrLn, stderr)
import Data.Either (lefts, rights)

-- exceptions

data NasaException =
    NasaYamlParseException FilePath ParseException -- ^ An exception to represent YAML parsing errors
    deriving Show

instance Exception NasaException

renderNasaException :: NasaException -> String
renderNasaException (NasaYamlParseException fn e) =
    "Could not parse YAML file " ++ fn ++ ": " ++ show e

-- data types

data ListOptions = ListOptions
    { _inList :: FilePath
    }

data Options = CmdList ListOptions

data NasaYamlStruct = NasaYamlStruct {
      _nasaYamlID :: Integer
    , _nasaYamlTitle :: String
} deriving (Show, Eq)

instance FromJSON NasaYamlStruct where
    parseJSON = withObject "NasaYamlStruct" $ \v -> NasaYamlStruct
        <$> v .:   "id"
        <*> v .:   "title"

instance ToJSON NasaYamlStruct where
    toJSON x = object [
        "id"    .= _nasaYamlID x,
        "title" .= _nasaYamlTitle x
        ]

-- command line interface

main :: IO ()
main = do
    cmdOpts <- OP.customExecParser p optParserInfo
    catch (runCmd cmdOpts) handler
    where
        p = OP.prefs OP.showHelpOnEmpty
        handler :: NasaException -> IO ()
        handler e = do
            hPutStrLn stderr $ renderNasaException e
            exitFailure

runCmd :: Options -> IO ()
runCmd o = case o of
    CmdList opts -> runList opts

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (OP.helper <*> versionOption <*> optParser) (
    OP.briefDesc <>
    OP.progDesc "nasa"
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

parseFilePath :: OP.Parser FilePath
parseFilePath = OP.strOption (
    OP.long "baseDir" <>
    OP.short 'd' <>
    OP.help "root directory where to search for NASA modules"
    )

-- program logic

runList :: ListOptions -> IO ()
runList (ListOptions baseDir) = do
    yamlCollection <- readNasaModuleCollection baseDir
    hPutStrLn stderr $ show $ map _nasaYamlID yamlCollection

readNasaModuleCollection :: FilePath -> IO [NasaYamlStruct]
readNasaModuleCollection baseDir = do
    hPutStrLn stderr "Searching NASA.yml files... "
    yamlFilePaths <- findAllNasaYamlFiles baseDir
    hPutStrLn stderr $ show (length yamlFilePaths) ++ " found"
    hPutStrLn stderr "Loading NASA modules... "
    eitherYamls <- mapM (try . readNasaYaml) yamlFilePaths :: IO [Either NasaException NasaYamlStruct]
    unless (null . lefts $ eitherYamls) $ do
        hPutStrLn stderr "Some modules were skipped:"
        forM_ (zip yamlFilePaths eitherYamls) $ \(posF, epac) -> do
            case epac of
                Left e -> do
                    hPutStrLn stderr (renderNasaException e)
                _ -> return ()
    let loadedYamlFiles = rights eitherYamls
    hPutStrLn stderr $ "Modules loaded: " ++ (show . length $ loadedYamlFiles)
    return loadedYamlFiles

findAllNasaYamlFiles :: FilePath -> IO [FilePath]
findAllNasaYamlFiles baseDir = do
    entries <- listDirectory baseDir
    let curFiles = map (baseDir </>) $ filter (=="NASA.yml") $ map takeFileName entries
    subDirs <- filterM doesDirectoryExist . map (baseDir </>) $ entries
    moreFiles <- fmap concat . mapM findAllNasaYamlFiles $ subDirs
    return $ curFiles ++ moreFiles

readNasaYaml :: FilePath -> IO NasaYamlStruct
readNasaYaml yamlPath = do
    yamlRaw <- B.readFile yamlPath
    case decodeEither' yamlRaw of
        Left err  -> throwIO $ NasaYamlParseException yamlPath err
        Right pac -> return pac
