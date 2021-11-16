module NASSA.Utils where

import           Data.Yaml                  (ParseException, prettyPrintParseException)
import           Control.Exception          (Exception)
import GHC.Base (String)

data NassaException =
    NassaYamlParseException FilePath ParseException -- ^ An exception to represent YAML parsing errors
    deriving Show

instance Exception NassaException

renderNassaException :: NassaException -> String
renderNassaException (NassaYamlParseException fn e) =
    "/!\\ YAML file " ++ fn ++ " could not be parsed: " ++ prettyPrintParseException e