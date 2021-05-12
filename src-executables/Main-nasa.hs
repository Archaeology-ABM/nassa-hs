{-# LANGUAGE OverloadedStrings #-}

import           Paths_nasa                 (version)

import           Control.Applicative        ((<|>))
import           Control.Exception          (Exception, throwIO, catch)
import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             (.:), (.:?), (.=))
import qualified Data.ByteString            as B                                             
import           Data.Version               (showVersion)
import           Data.Yaml                  (decodeEither', ParseException)
import qualified Options.Applicative        as OP
import           System.Exit                (exitFailure)
import           System.IO                  (hPutStrLn, stderr)

-- data types

data NasaException = 
    NasaYamlParseException FilePath ParseException -- ^ An exception to represent YAML parsing errors
    deriving Show

instance Exception NasaException

renderNasaException :: NasaException -> String
renderNasaException (NasaYamlParseException fn e) =
    "Could not parse YAML file " ++ fn ++ ": " ++ show e

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
    yaml <- readNasaYaml baseDir
    hPutStrLn stderr $ show $ _nasaYamlID yaml
    
readNasaYaml :: FilePath -> IO NasaYamlStruct
readNasaYaml yamlPath = do
    yamlRaw <- B.readFile yamlPath
    case decodeEither' yamlRaw of
        Left err  -> throwIO $ NasaYamlParseException yamlPath err
        Right pac -> return pac