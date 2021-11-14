{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module NASSA.Types where

import           Data.Aeson                 (FromJSON,
                                             parseJSON, withObject,
                                             (.:), (.:?), withText)

data NassaInteractionsStruct = NassaInteractionsStruct {
      _nassaInteractionsDependencies :: Maybe [Int],
      _nassaInteractionsSuggests :: Maybe [Int]
    } deriving (Show, Eq)

instance FromJSON NassaInteractionsStruct where
    parseJSON = withObject "NassaInteractionsStruct" $ \v -> NassaInteractionsStruct
        <$> v .:   "dependencies"
        <*> v .:   "suggests"

data NassaLanguageStruct = 
      LanguageR 
    | LanguagePython
    | LanguageNetlogo
    deriving (Eq)

instance Show NassaLanguageStruct where
    show LanguageR = "R"
    show LanguagePython = "Python"
    show LanguageNetlogo = "Netlogo"

instance FromJSON NassaLanguageStruct where
    parseJSON = withText "language" $ \case
        "R"         -> pure LanguageR
        "Python"    -> pure LanguagePython
        "Netlogo"   -> pure LanguageNetlogo
        _           -> fail "unknown Language"

data NassaYamlStruct = NassaYamlStruct {
      _nassaYamlID :: Integer
    , _nassaYamlTitle :: String
    , _nassaYamlCategory :: String
    , _nassaYamlTags :: Maybe [String]
    , _nassaYamlAuthorship :: String
    , _nassaYamlLanguage :: NassaLanguageStruct
    , _nassaYamlLicense :: Maybe String
    , _nassaYamlInteractions :: Maybe NassaInteractionsStruct
    } deriving (Show, Eq)

instance FromJSON NassaYamlStruct where
    parseJSON = withObject "NassaYamlStruct" $ \v -> NassaYamlStruct
        <$> v .:   "id"
        <*> v .:   "title"
        <*> v .:   "category"
        <*> v .:?  "tags"
        <*> v .:   "authorship"
        <*> v .:   "language"
        <*> v .:?  "license"
        <*> v .:?  "interactions"