module NASSA.Utils where

import           Data.Yaml                  (ParseException)
import           Control.Exception          (Exception)

data NassaException =
    NassaYamlParseException FilePath ParseException -- ^ An exception to represent YAML parsing errors
    deriving Show

instance Exception NassaException

renderNassaException :: NassaException -> String
renderNassaException (NassaYamlParseException fn e) =
    "Could not parse YAML file " ++ fn ++ ": " ++ show e