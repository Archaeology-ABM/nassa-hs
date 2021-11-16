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
    , _nassaYamlRelatedReferences :: Maybe [String]
    , _nassaYamlDomainKeywords :: Maybe DomainKeyword
    , _nassaYamlModellingKeywords :: [String]
    , _nassaYamlProgrammingKeywords :: [String]
    , _nassaYamlProgrammingLanguage :: ProgrammingLanguage
    , _nassaYamlSoftwareDependencies :: [String]
    , _nassaYamlDocsCheckList :: DocsCheckList
    , _nassaYamlReadmeFile :: Maybe FilePath
    , _nassaYamlDocsDir :: Maybe FilePath
    , _nassaYamlDesignDetailsFile :: Maybe FilePath
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
        <*> v .:? "domainKeywords"
        <*> v .:  "modellingKeywords"
        <*> v .:  "programmingKeywords"
        <*> v .:  "programmingLanguages"
        <*> v .:  "softwareDependencies"
        <*> v .:  "docsCheckList"
        <*> v .:? "readmeFile"
        <*> v .:? "docsDir"
        <*> v .:? "designDetailsFile"
        <*> v .:? "license"

data Contributor = Contributor
    { _contributorName  :: String
    , _contributorEmail :: String
    , _contributorORCID :: String
    }
    deriving (Show, Eq)

instance FromJSON Contributor where
    parseJSON = withObject "contributors" $ \v -> Contributor
        <$> v .:  "name"
        <*> v .:  "email"
        <*> v .:  "orcid"

data DomainKeyword = DomainKeyword
    { _keywordSubjects :: Maybe [String]
    , _keywordRegions :: Maybe [String]
    , _keywordPeriods :: Maybe [String]
    }
    deriving (Show, Eq)

instance FromJSON DomainKeyword where
    parseJSON = withObject "domainKeywords" $ \v -> DomainKeyword
        <$> v .:? "subjects"
        <*> v .:? "regions"
        <*> v .:? "periods"

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

data DocsCheckList = DocsCheckList
    { _docsCheckListCommentaryInCode :: Bool
    , _docsCheckListPseudocodeText :: Bool
    , _docsCheckListPseudocodeGraph :: Bool
    }
    deriving (Show, Eq)

instance FromJSON DocsCheckList where
    parseJSON = withObject "docsCheckList" $ \v -> DocsCheckList
        <$> v .:  "commentaryInCode"
        <*> v .:  "pseudocodeText"
        <*> v .:  "pseudocodeGraph"
