module NASSA.CLI.Validate where

import           NASSA.ReadYml

import           Control.Monad (unless)
import           System.Exit   (exitFailure, exitSuccess)
import           System.IO     (hPutStrLn, stdout)

data ValidateOptions = ValidateOptions {
      _validateBaseDir       :: FilePath
    , _validateNoExitCode    :: Bool
    , _validateIgnoreVersion :: Bool
    }

runValidate :: ValidateOptions -> IO ()
runValidate (ValidateOptions baseDir noExitCode ignoreVersion) = do
    nassaYmlFiles <- findAllNassaYamlFiles baseDir
    yamlCollection <- readNassaModuleCollection ignoreVersion baseDir
    let numberOfNASSAymlFiles = length nassaYmlFiles
        numberOfLoadedModules = length yamlCollection
    if numberOfNASSAymlFiles == numberOfLoadedModules
    then do
        hPutStrLn stdout "Validation passed: OK"
        unless noExitCode exitSuccess
    else do
        hPutStrLn stdout "Validation failed: ERROR"
        unless noExitCode exitFailure
