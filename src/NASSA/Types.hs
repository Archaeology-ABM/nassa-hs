{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module NASSA.Types where

import           NASSA.SPDXLicense

import           Control.Applicative ((<|>))
import           Control.Monad       (guard, mzero)
import           Data.Aeson          (FromJSON, ToJSON (..), Value (String),
                                      parseJSON, withObject, withText, (.:),
                                      (.:?))
import           Data.Char           (digitToInt, toLower)
import           Data.List           (intercalate)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TS
import           Data.Time           (Day)
import           Data.Version        (Version, makeVersion, showVersion)
import           System.FilePath     (takeExtension)
import qualified Text.Email.Validate as TEV
import qualified Text.Parsec         as P
import qualified Text.Parsec.Text    as P

newtype NassaModule = NassaModule (FilePath, NassaModuleYamlStruct)
    deriving (Show, Eq)

data NassaModuleYamlStruct = NassaModuleYamlStruct {
      _nassaYamlID                  :: ModuleID
    , _nassaNASSAVersion            :: NassaVersion
    , _nassaYamlModuleType          :: ModuleType
    , _nassaYamlTitle               :: ModuleTitle
    , _nassaYamlModuleVersion       :: Version
    , _nassaYamlContributors        :: [Contributor]
    , _nassaYamlLastUpdateDate      :: Day
    , _nassaYamlDescription         :: ModuleDescription
    , _nassaYamlCoverImagePath      :: Maybe ModuleImagePath
    , _nassaYamlRelatedModules      :: Maybe [ModuleID]
    , _nassaYamlReferences          :: Maybe ReferenceStruct
    , _nassaYamlDomainKeywords      :: Maybe DomainKeyword
    , _nassaYamlModellingKeywords   :: [String]
    , _nassaYamlProgrammingKeywords :: [String]
    , _nassaYamlImplementations     :: [Implementation]
    , _nassaYamlInputs              :: Maybe [ModuleInput]
    , _nassaYamlOutputs             :: Maybe [ModuleOutput]
    -- , _nassaYamlDocsCheckList :: DocsCheckList
    , _nassaYamlDocsDir             :: Maybe FilePath
    , _nassaYamlLicense             :: Maybe SPDXLicense
    } deriving (Show, Eq)

instance FromJSON NassaModuleYamlStruct where
    parseJSON = withObject "NassaModuleYamlStruct" $ \v -> NassaModuleYamlStruct
        <$> v .:  "id"
        <*> v .:  "nassaVersion"
        <*> v .:  "moduleType"
        <*> v .:  "title"
        <*> v .:  "moduleVersion"
        <*> v .:  "contributors"
        <*> v .:  "lastUpdateDate"
        <*> v .:  "description"
        <*> v .:? "coverImage"
        <*> v .:? "relatedModules"
        <*> v .:? "references"
        <*> v .:? "domainKeywords"
        <*> v .:  "modellingKeywords"
        <*> v .:  "programmingKeywords"
        <*> v .:  "implementations"
        <*> v .:? "inputs"
        <*> v .:? "outputs"
        -- <*> v .:  "docsCheckList"
        <*> v .:? "docsDir"
        <*> v .:? "license"


newtype ModuleID = ModuleID String
    deriving (Eq)

instance Show ModuleID where
    show (ModuleID s) = s

instance FromJSON ModuleID where
    parseJSON (String s) = pure $ ModuleID $ T.unpack s
    parseJSON _          = mzero

type NassaVersion = Version

validNassaVersions :: [NassaVersion]
validNassaVersions = map makeVersion [[1,0,0]]

latestNassaVersion :: NassaVersion
latestNassaVersion = last validNassaVersions

showNassaVersion :: NassaVersion -> String
showNassaVersion = showVersion

newtype ModuleTitle = ModuleTitle String
    deriving (Eq)

instance Show ModuleTitle where
    show (ModuleTitle s) = s

instance FromJSON ModuleTitle where
    parseJSON (String s) =
        if T.length s <= 50
        then pure $ ModuleTitle $ T.unpack s
        else fail "module title must not be longer than 50 characters"
    parseJSON _ = mzero

newtype ModuleDescription = ModuleDescription String
    deriving (Eq)

instance Show ModuleDescription where
    show (ModuleDescription s) = s

instance FromJSON ModuleDescription where
    parseJSON (String s) =
        if T.length s <= 300
        then pure $ ModuleDescription $ T.unpack s
        else fail "module description must not be longer than 300 characters"
    parseJSON _ = mzero

newtype ModuleImagePath = ModuleImagePath FilePath
    deriving (Eq)

instance Show ModuleImagePath where
    show (ModuleImagePath p) = p

instance FromJSON ModuleImagePath where
    parseJSON (String s) =
        let path = T.unpack s
        in if map toLower (takeExtension path) `elem` [".png", ".jpg", ".jpeg", ".svg"]
           then pure $ ModuleImagePath $ path
           else fail "cover image must have extension .png, .jpg, .jpeg, or .svg"
    parseJSON _ = mzero

data ModuleType =
      Algorithm
    | Submodel
    deriving (Eq)

instance Show ModuleType where
    show Algorithm = "Algorithm"
    show Submodel  = "Submodel"

instance FromJSON ModuleType where
    parseJSON = withText "programmingLanguage" $ \case
        "Algorithm" -> pure Algorithm
        "Submodel"  -> pure Submodel
        other       -> fail $ "unknown module type: " ++ show other

data Contributor = Contributor
    { _contributorRole  :: [Role]
    , _contributorName  :: String
    , _contributorEmail :: Email
    , _contributorORCID :: ORCID
    }
    deriving (Show, Eq)

instance FromJSON Contributor where
    parseJSON = withObject "contributors" $ \v -> Contributor
        <$> v .: "roles"
        <*> v .: "name"
        <*> v .: "email"
        <*> v .: "orcid"

data Role =
    RoleAuthor -- ^ Full authors who have made substantial contributions to the package and should show up in the package citation.
  | RoleCompiler -- ^ Persons who collected code (potentially in other languages) but did not make further substantial contributions to the package.
  | RoleContributor -- ^ Authors who have made smaller contributions (such as code patches etc.) but should not show up in the package citation.
  | RoleCopyrightHolder -- ^ Copyright holders.
  | RoleCreator -- ^ Package maintainer.
  | RoleThesisAdvisor -- ^ Thesis advisor, if the package is part of a thesis.
  | RoleTranslator -- ^ Translator from one programming language to the other
  deriving (Eq)

instance Show Role where
    show RoleAuthor          = "Author"
    show RoleCompiler        = "Compiler"
    show RoleContributor     = "Contributor"
    show RoleCopyrightHolder = "Copyright Holder"
    show RoleCreator         = "Creator"
    show RoleThesisAdvisor   = "Thesis Advisor"
    show RoleTranslator      = "Translator"

instance FromJSON Role where
    parseJSON = withText "role" $ \case
        "Author"           -> pure RoleAuthor
        "Compiler"         -> pure RoleCompiler
        "Contributor"      -> pure RoleContributor
        "Copyright Holder" -> pure RoleCopyrightHolder
        "Creator"          -> pure RoleCreator
        "Thesis Advisor"   -> pure RoleThesisAdvisor
        "Translator"       -> pure RoleTranslator
        other              -> fail $ "unknown role: " ++ show other

newtype Email = Email TEV.EmailAddress
    deriving (Show, Eq)

-- https://hackage.haskell.org/package/email-validate-json-0.1.0.0
instance FromJSON Email where
    parseJSON (String s) = case TEV.emailAddress (TS.encodeUtf8 s) of
        Nothing -> fail "not a valid email address"
        Just x  -> pure $ Email x
    parseJSON _ = mzero

-- | A data type to represent an ORCID
-- see https://support.orcid.org/hc/en-us/articles/360006897674-Structure-of-the-ORCID-Identifier
data ORCID = ORCID
    { _orcidNums     :: [Char]
    , _orcidChecksum :: Char
    }
    deriving (Show, Eq)

instance FromJSON ORCID where
    parseJSON (String s) = case P.runParser parseORCID () "" s of
        Left err -> fail $ show err
        Right x  -> pure x
    parseJSON _          = mzero

instance ToJSON ORCID where
    toJSON x = String $ T.pack $ renderORCID x

parseORCID :: P.Parser ORCID
parseORCID = do
    orcid <- (\a b c d e -> ORCID (concat [a,b,c,d]) e) <$>
            fourBlock <* m
        <*> fourBlock <* m
        <*> fourBlock <* m
        <*> threeBlock <*> checksumDigit <* P.eof
    guard (validateORCID orcid) P.<?> "ORCID is not valid"
    return orcid
  where
      fourBlock = P.count 4 P.digit
      m = P.oneOf "-"
      threeBlock = P.count 3 P.digit
      checksumDigit = P.digit P.<|> P.char 'X'

validateORCID :: ORCID -> Bool
validateORCID (ORCID nums check) =
    let numsInt = map digitToInt nums
        total = makeTotal 0 numsInt
        remainder = total `mod` 11
        result = (12 - remainder) `mod` 11
        checkInt = if check == 'X' then 10 else digitToInt check
    in result == checkInt
    where
        makeTotal :: Int -> [Int] -> Int
        makeTotal a []     = a
        makeTotal a (x:xs) = makeTotal ((a + x) * 2) xs

renderORCID :: ORCID -> String
renderORCID (ORCID nums check) =
    intercalate "-" (chunks 4 nums) ++ [check]
    where
        chunks :: Int -> [a] -> [[a]]
        chunks _ [] = []
        chunks n xs =
            let (ys, zs) = splitAt n xs
            in  ys : chunks n zs

data ReferenceStruct = ReferenceStruct
    { _referencesModuleReferences     :: Maybe [String]
    , _referencesUseExampleReferences :: Maybe [String]
    }
    deriving (Show, Eq)

instance FromJSON ReferenceStruct where
    parseJSON = withObject "references" $ \v -> ReferenceStruct
        <$> v .:? "moduleReferences"
        <*> v .:? "useExampleReferences"

data DomainKeyword = DomainKeyword
    { _keywordSubjects :: Maybe [String]
    , _keywordRegions  :: Maybe [String]
    , _keywordPeriods  :: Maybe [String]
    }
    deriving (Show, Eq)

instance FromJSON DomainKeyword where
    parseJSON = withObject "domainKeywords" $ \v -> DomainKeyword
        <$> v .:? "subjects"
        <*> v .:? "regions"
        <*> v .:? "periods"

data Implementation = Implementation
    { _implementationLanguage        :: ProgrammingLanguage
    , _nassaYamlSoftwareDependencies :: [String]
    }
    deriving (Show, Eq)

instance FromJSON Implementation where
    parseJSON = withObject "implementations" $ \v -> Implementation
        <$> v .: "language"
        <*> v .: "softwareDependencies"

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
    show LanguageR          = "R"
    show LanguagePython     = "Python"
    show LanguageNetLogo    = "NetLogo"
    show LanguageJava       = "Java"
    show LanguageJulia      = "Julia"
    show LanguageCsharp     = "C#"
    show LanguageRuby       = "Ruby"
    show LanguageProcessing = "Processing"

langInPathName :: ProgrammingLanguage -> String
langInPathName LanguageR          = "r"
langInPathName LanguagePython     = "python"
langInPathName LanguageNetLogo    = "netlogo"
langInPathName LanguageJava       = "java"
langInPathName LanguageJulia      = "julia"
langInPathName LanguageCsharp     = "csharp"
langInPathName LanguageRuby       = "ruby"
langInPathName LanguageProcessing = "processing"

instance FromJSON ProgrammingLanguage where
    parseJSON = withText "programmingLanguage" $ \case
        "R"          -> pure LanguageR
        "Python"     -> pure LanguagePython
        "NetLogo"    -> pure LanguageNetLogo
        "Java"       -> pure LanguageJava
        "Julia"      -> pure LanguageJulia
        "C#"         -> pure LanguageCsharp
        "Ruby"       -> pure LanguageRuby
        "Processing" -> pure LanguageProcessing
        other        -> fail $ "unknown Language: " ++ show other

data ModuleInput = ModuleInput
    { _inputName        :: String
    , _inputType        :: Maybe String
    , _inputUnit        :: Maybe String
    , _inputDefault     :: Maybe String
    , _inputDescription :: Maybe String
    }
    deriving (Show, Eq)

showInt :: Int -> String
showInt = show
showDouble :: Double -> String
showDouble = show

instance FromJSON ModuleInput where
    parseJSON = withObject "inputs" $ \v -> ModuleInput
        <$> v .:  "name"
        <*> v .:? "type"
        <*> v .:? "unit"
        -- this is to cover the case that the default value is a number
        <*> (v .:? "default" <|>
              fmap showInt <$> v .:? "default" <|>
              fmap showDouble <$> v .:? "default"
            )
        <*> v .:? "description"

data ModuleOutput = ModuleOutput
    { _outputName        :: String
    , _outputType        :: Maybe String
    , _outputUnit        :: Maybe String
    , _outputDescription :: Maybe String
    }
    deriving (Show, Eq)

instance FromJSON ModuleOutput where
    parseJSON = withObject "outputs" $ \v -> ModuleOutput
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
