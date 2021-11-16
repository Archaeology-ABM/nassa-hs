import           NASSA.CLI.List
import           NASSA.Utils
import           Paths_nassa                 (version)

import           Control.Exception          (catch)
import           Data.Version               (showVersion)
import qualified Options.Applicative        as OP
import           System.Exit                (exitFailure)
import           System.IO                  (hPutStrLn, stderr)


data Options = CmdList ListOptions

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
    OP.progDesc "nassa searches, validates and lists NASSA modules"
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
