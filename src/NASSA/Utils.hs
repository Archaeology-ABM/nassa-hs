module NASSA.Utils where

import           NASSA.Types                (ModuleID)

import           Data.Yaml                  (ParseException, prettyPrintParseException)
import           Control.Exception          (Exception)

data NassaException =
      NassaYamlParseException FilePath ParseException -- ^ An exception to represent YAML parsing errors
    | NassaModuleIntegrityException ModuleID String -- ^ An exception to represent structural issues in a NASSA module
    deriving Show

instance Exception NassaException

renderNassaException :: NassaException -> String
renderNassaException (NassaYamlParseException fn e) =
    "/!\\ YAML file " ++ fn ++ " could not be parsed: " ++ prettyPrintParseException e
renderNassaException (NassaModuleIntegrityException moID s) =
    "/!\\ Module " ++ show moID ++ " is corrupted: " ++ s