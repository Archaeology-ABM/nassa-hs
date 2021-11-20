module NASSA.Utils where

import           Data.Yaml                  (ParseException, prettyPrintParseException)
import           Control.Exception          (Exception)

data NassaException =
      NassaYamlParseException FilePath ParseException -- ^ An exception to represent YAML parsing errors
    | NassaModuleIntegrity String String -- ^ An exception to represent structural issues in a NASSA module
    deriving Show

instance Exception NassaException

renderNassaException :: NassaException -> String
renderNassaException (NassaYamlParseException fn e) =
    "/!\\ YAML file " ++ fn ++ " could not be parsed: " ++ prettyPrintParseException e
renderNassaException (NassaModuleIntegrity moID s) =
    "/!\\ Module " ++ moID ++ " is corrupted: " ++ s