import           NASSA.CLI.List
import           NASSA.CLI.Validate
import           NASSA.Types
import           NASSA.Utils
import           Paths_nassa         (version)

import           Control.Exception   (catch)
import           Data.List           (intercalate)
import           Data.Version        (showVersion)
import qualified Options.Applicative as OP
import           System.Exit         (exitFailure)
import           System.IO           (hPutStrLn, stderr)


data Options =
    CmdList ListOptions
  | CmdValidate ValidateOptions

-- command line interface

main :: IO ()
main = do
    hPutStrLn stderr renderVersion
    hPutStrLn stderr ""
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
    CmdList opts     -> runList opts
    CmdValidate opts -> runValidate opts

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (OP.helper <*> versionOption <*> optParser) (
    OP.briefDesc <>
    OP.progDesc "nassa searches, validates and lists NASSA modules"
    )

versionOption :: OP.Parser (a -> a)
versionOption = OP.infoOption (showVersion version) (OP.long "version" <> OP.help "Show version")

renderVersion :: String
renderVersion =
    "nassa v" ++ showVersion version ++ " for the NASSA standard v" ++
    intercalate ", v" (map showNassaVersion validNassaVersions)-- ++ "\n" ++
    --"https://..."

optParser :: OP.Parser Options
optParser = OP.subparser (
        OP.command "list" listOptInfo <>
        OP.command "validate" validateOptInfo <>
        OP.commandGroup "Inspection commands:"
    )
  where
    listOptInfo = OP.info (OP.helper <*> (CmdList <$> listOptParser))
        (OP.progDesc "List nassa modules")
    validateOptInfo = OP.info (OP.helper <*> (CmdValidate <$> validateOptParser))
        (OP.progDesc "Check nassa modules for structural correctness")

listOptParser :: OP.Parser ListOptions
listOptParser = ListOptions <$> parseBasePath
                            <*> parseRawOutput

validateOptParser :: OP.Parser ValidateOptions
validateOptParser = ValidateOptions <$> parseBasePath
                                    <*> parseNoExitCode

parseBasePath :: OP.Parser FilePath
parseBasePath = OP.strOption (
    OP.long "baseDir" <>
    OP.short 'd' <>
    OP.help "root directory where to search for NASSA modules"
    )

parseRawOutput :: OP.Parser Bool
parseRawOutput = OP.switch (
    OP.long "raw" <>
    OP.help "output table as tsv without header. Useful for piping into grep or awk"
    )

parseNoExitCode :: OP.Parser Bool
parseNoExitCode = OP.switch (
    OP.long "noExitCode" <>
    OP.help "do not produce an explicit exit code" <>
    OP.hidden
    )
