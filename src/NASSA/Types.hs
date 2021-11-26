{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module NASSA.Types where

import           Control.Monad              (mzero)
import           Data.Aeson                 (FromJSON,
                                             parseJSON, withObject,
                                             (.:), (.:?), withText, 
                                             Value (String))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TS
import           Data.Time                  (Day)
import           Data.Version               (Version)
import qualified Text.Parsec                as P
import qualified Text.Parsec.Text           as P
import qualified Text.Email.Validate        as TEV



newtype NassaModule = NassaModule (FilePath, NassaYamlStruct)
    deriving (Show, Eq)

data NassaYamlStruct = NassaYamlStruct {
      _nassaYamlID :: String
    , _nassaYamlTitle :: ModuleTitle
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
    , _nassaYamlInputs :: Maybe [InOrOutput]
    , _nassaYamlOutputs :: Maybe [InOrOutput]
    -- , _nassaYamlDocsCheckList :: DocsCheckList
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
        <*> v .:? "inputs"
        <*> v .:? "outputs"
        -- <*> v .:  "docsCheckList"
        <*> v .:? "readmeFile"
        <*> v .:? "docsDir"
        <*> v .:? "designDetailsFile"
        <*> v .:? "license"

newtype ModuleTitle = ModuleTitle String
    deriving (Eq)

instance Show ModuleTitle where
    show (ModuleTitle s) = s

instance FromJSON ModuleTitle where
    parseJSON (String s) =
        if T.length s <= 100
        then pure $ ModuleTitle $ T.unpack s
        else fail "module title must not be longer than 100 characters"
    parseJSON _ = mzero

data Contributor = Contributor
    { _contributorName  :: String
    , _contributorEmail :: Email
    , _contributorORCID :: ORCID
    }
    deriving (Show, Eq)

instance FromJSON Contributor where
    parseJSON = withObject "contributors" $ \v -> Contributor
        <$> v .: "name"
        <*> v .: "email"
        <*> v .: "orcid"

newtype Email = Email TEV.EmailAddress
    deriving (Show, Eq)

-- https://hackage.haskell.org/package/email-validate-json-0.1.0.0
instance FromJSON Email where
    parseJSON (String s) = case TEV.emailAddress (TS.encodeUtf8 s) of
        Nothing -> fail "not a valid email address"
        Just x  -> pure $ Email x
    parseJSON _ = mzero

newtype ORCID = ORCID String
    deriving (Show, Eq)

instance FromJSON ORCID where
    parseJSON (String s) = case P.runParser parseORCID () "" s of
        Left err -> fail $ show err
        Right x  -> pure x
    parseJSON _          = mzero

parseORCID :: P.Parser ORCID
parseORCID = do
  s <- (\a b c d -> [a,b,c,d]) <$> nums <* m 
                               <*> nums <* m 
                               <*> nums <* m 
                               <*> nums <* P.eof
  return $ ORCID $ concat s
  where
      nums = P.count 4 P.digit
      m = P.oneOf "-"

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
    | LanguageNetLogo
    | LanguageJava
    | LanguageJulia
    | LanguageCsharp
    | LanguageRuby
    | LanguageProcessing
    deriving (Eq)

instance Show ProgrammingLanguage where
    show LanguageR = "R"
    show LanguagePython = "Python"
    show LanguageNetLogo = "NetLogo"
    show LanguageJava = "Java"
    show LanguageJulia = "Julia"
    show LanguageCsharp = "C#"
    show LanguageRuby = "Ruby"
    show LanguageProcessing = "Processing"

instance FromJSON ProgrammingLanguage where
    parseJSON = withText "programmingLanguage" $ \case
        "R"         -> pure LanguageR
        "Python"    -> pure LanguagePython
        "NetLogo"   -> pure LanguageNetLogo
        "Java"   -> pure LanguageJava
        "Julia"   -> pure LanguageJulia
        "C#"   -> pure LanguageCsharp
        "Ruby"   -> pure LanguageRuby
        "Processing"   -> pure LanguageProcessing
        other       -> fail $ "unknown Language: " ++ show other

data InOrOutput = InOrOutput
    { _inOrOutputName :: String
    , _inOrOutputType :: Maybe String
    , _inOrOutputUnit :: Maybe String
    , _inOrOutputDescription :: Maybe String
    }
    deriving (Show, Eq)

instance FromJSON InOrOutput where
    parseJSON = withObject "inputs or outputs" $ \v -> InOrOutput
        <$> v .:  "name"
        <*> v .:? "type"
        <*> v .:? "unit"
        <*> v .:? "description"

-- data DocsCheckList = DocsCheckList
--     { _docsCheckListCommentaryInCode :: Bool
--     , _docsCheckListPseudocodeText :: Bool
--     , _docsCheckListPseudocodeGraph :: Bool
--     }
--     deriving (Show, Eq)

-- instance FromJSON DocsCheckList where
--     parseJSON = withObject "docsCheckList" $ \v -> DocsCheckList
--         <$> v .:  "commentaryInCode"
--         <*> v .:  "pseudocodeText"
--         <*> v .:  "pseudocodeGraph"
