{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module NASSA.Types where

import           Data.Aeson                 (FromJSON,
                                             parseJSON, withObject,
                                             (.:), (.:?), withText)
import           Data.Time                  (Day)
import           Data.Version               (Version)


data NassaYamlStruct = NassaYamlStruct {
      _nassaYamlID :: String
    , _nassaYamlTitle :: String
    , _nassaYamlModuleVersion :: Version
    , _nassaYamlContributors :: [Contributor]
    , _nassaYamlLastUpdateDate :: Day
    , _nassaYamlDescription :: String
    , _nassaRelatedReferences :: Maybe [String]
    , _nassaYamlProgrammingLanguage :: [ProgrammingLanguage]
    , _nassaYamlSoftwareDependencies :: [String]
    -- , _nassaYamlInteractions :: Maybe NassaInteractionsStruct
    , _nassaYamlLicense :: Maybe String
    } deriving (Show, Eq)

instance FromJSON NassaYamlStruct where
    parseJSON = withObject "NassaYamlStruct" $ \v -> NassaYamlStruct
        <$> v .:  "id"
        <*> v .:  "title"
        <*> v .:  "moduleVersion"
        <*> v .:  "contributors"
        <*> v .:  "lastUpdateDate"
        <*> v .:  "description"
        <*> v .:? "relatedReferences"
        <*> v .:  "programmingLanguages"
        <*> v .:  "softwareDependencies"
        -- <*> v .:?  "interactions"
        <*> v .:?  "license"

data Contributor = Contributor
    { contributorName  :: String
    , contributorEmail :: String
    , contributorORCID :: Maybe String
    }
    deriving (Show, Eq)

instance FromJSON Contributor where
    parseJSON = withObject "contributors" $ \v -> Contributor
        <$> v .:  "name"
        <*> v .:  "email"
        <*> v .:? "orcid"


data NassaInteractionsStruct = NassaInteractionsStruct {
      _nassaInteractionsDependencies :: Maybe [Int],
      _nassaInteractionsSuggests :: Maybe [Int]
    } deriving (Show, Eq)

instance FromJSON NassaInteractionsStruct where
    parseJSON = withObject "NassaInteractionsStruct" $ \v -> NassaInteractionsStruct
        <$> v .:   "dependencies"
        <*> v .:   "suggests"

data ProgrammingLanguage = 
      LanguageR 
    | LanguagePython
    | LanguageNetlogo
    deriving (Eq)

instance Show ProgrammingLanguage where
    show LanguageR = "R"
    show LanguagePython = "Python"
    show LanguageNetlogo = "Netlogo"

instance FromJSON ProgrammingLanguage where
    parseJSON = withText "programmingLanguage" $ \case
        "R"         -> pure LanguageR
        "Python"    -> pure LanguagePython
        "Netlogo"   -> pure LanguageNetlogo
        other       -> fail $ "unknown Language: " ++ show other
