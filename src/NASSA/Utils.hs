module NASSA.Utils where

import           NASSA.Types                (ModuleID)

import           Data.Yaml                  (ParseException, prettyPrintParseException)
import           Control.Exception          (Exception)

data NassaException =
      NassaYamlParseException FilePath ParseException -- ^ An exception to represent YAML parsing errors
    | NassaModuleIntegrityException ModuleID String -- ^ An exception to represent structural issues in a NASSA module
    | NassaBibTeXException FilePath String -- ^ An exception to represent errors when trying to parse the .bib file
    | NassaModuleMissingVersionException FilePath -- ^ An exception to indicate a missing nassaVersion field
    | NassaModuleVersionException FilePath String -- ^ An exception to represent an issue with the NASSA version of a module
    deriving Show

instance Exception NassaException

renderNassaException :: NassaException -> String
renderNassaException (NassaYamlParseException fn e) =
    "/!\\ YAML file " ++ fn ++ " could not be parsed: " ++ prettyPrintParseException e
renderNassaException (NassaModuleIntegrityException moID s) =
    "/!\\ Module " ++ show moID ++ " is corrupted: " ++ s
renderNassaException (NassaBibTeXException f s) =
    "/!\\ BibTex file " ++ f ++ " broken: " ++ s
renderNassaException (NassaModuleMissingVersionException p) =
    "/!\\ The NASSA.yml file " ++ show p ++ " has no nassaVersion field. " ++
    "This is mandatory."
renderNassaException (NassaModuleVersionException p s) =
    "/!\\ NASSA standard version mismatch in " ++ show p ++
    ". It has the nassaVersion \"" ++ s ++ "\", which is not supported by this version of the nassa tool."
