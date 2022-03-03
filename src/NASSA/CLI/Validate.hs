module NASSA.CLI.Validate where

import           NASSA.ReadYml

import           Control.Monad            (unless)
import           System.Exit              (exitFailure, exitSuccess)
import           System.IO                (hPutStrLn, stdout)

data ValidateOptions = ValidateOptions { 
      _validateBaseDir :: FilePath
    , _validateNoExitCode   :: Bool
    }

runValidate :: ValidateOptions -> IO ()
runValidate (ValidateOptions baseDir noExitCode) = do
    nassaYmlFiles <- findAllNassaYamlFiles baseDir
    yamlCollection <- readNassaModuleCollection baseDir
    let numberOfNASSAymlFiles = length nassaYmlFiles
        numberOfLoadedModules = length yamlCollection
    if numberOfNASSAymlFiles == numberOfLoadedModules
    then do
        hPutStrLn stdout "Validation passed ✓"
        unless noExitCode exitSuccess
    else do
        hPutStrLn stdout "Validation failed ✗"
        unless noExitCode exitFailure