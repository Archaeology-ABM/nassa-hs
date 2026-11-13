{-# LANGUAGE OverloadedStrings #-}

module NASSA.CLI.Track where

import           NASSA.History
import NASSA.ReadYml
import NASSA.ZipDir
import NASSA.Zenodo
import NASSA.Types

import System.Directory (doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>), takeDirectory)
import qualified Data.Set as S
import Control.Exception (finally)
import qualified Data.Text as T
import Data.Version (showVersion)
import Data.List (nub)

data TrackOptions = TrackOptions {
      _trackBaseDir         :: FilePath
    , _trackHistoryFilePath :: FilePath
    }

runTrack :: TrackOptions -> IO ()
runTrack (TrackOptions baseDir histFilePath) = do
    allModules <- readNassaModuleCollection False baseDir
    histFileExists <- doesFileExist histFilePath
    oldState <- if histFileExists
         then readHistory histFilePath
         else emptyHistory
    currentState <- makeHistory histFilePath allModules
    let diff = getNewModules currentState oldState
        updatedModuleIDs = S.map (ModuleID . modStateID) diff
        modulesToDeposit = filter (\(NassaModule _ yaml) -> _nassaYamlID yaml `S.member` updatedModuleIDs) allModules  
    -- using baseDir is problematic as things currently stand; the paths may be in relation to the histFile
    mapM_ (depositModuleAtZenodo baseDir) modulesToDeposit
   
    let newState = updateHistory oldState currentState
    writeHistory histFilePath newState
    
depositModuleAtZenodo :: FilePath -> NassaModule -> IO ()
depositModuleAtZenodo baseDir (NassaModule moduleDir yaml) = do
  zipArchivePath <- zipDirectoryToTemp (baseDir </> moduleDir)
  let cleanup = removeDirectoryRecursive (takeDirectory zipArchivePath)
      metadata = nassaToZenodoMetadata yaml
      upload = do
        result <- depositZip (sandboxClient token) metadata zipArchivePath
        print result
  upload `finally` cleanup
  where
    token = "..."

-- | convert a parsed NASSA YAML document to Zenodo metadata.
nassaToZenodoMetadata :: NassaModuleYamlStruct -> Metadata
nassaToZenodoMetadata yaml =
  Metadata {
      metadataTitle =
        T.pack (show (_nassaYamlTitle yaml))
    , metadataDescription =
        T.pack (show (_nassaYamlDescription yaml))
    , metadataPublicationDate =
        _nassaYamlLastUpdateDate yaml
    , metadataCreators =
        map contributorToCreator $  _nassaYamlContributors yaml
    , metadataUploadType =
        "software"
    , metadataPublicationType =
        Nothing
    , metadataAccessRight =
        "open"
    , metadataLicense =
        --T.pack . show <$> _nassaYamlLicense yaml
        Nothing
    , metadataKeywords =
        collectKeywords yaml
    , metadataVersion =
        Just $
          T.pack $
            showVersion (_nassaYamlModuleVersion yaml)
    }

contributorToCreator :: Contributor -> Creator
contributorToCreator contributor =
  Creator
    { creatorName = T.pack (_contributorName contributor)
    -- NASSA currently has no affiliation field.
    , creatorAffiliation = Nothing
    , creatorOrcid = T.pack . renderORCID  <$> _contributorORCID contributor
    }

collectKeywords :: NassaModuleYamlStruct -> [T.Text]
collectKeywords yaml =
  map T.pack $
    nub $
      baseKeywords
        ++ domainKeywords
        ++ _nassaYamlModellingKeywords yaml
        ++ _nassaYamlProgrammingKeywords yaml
        ++ implementationKeywords
  where
    baseKeywords =
      [ "NASSA"
      , "NASSA module"
      , "NASSA schema: " ++ showVersion (_nassaNASSAVersion yaml)
      , "Module type: " ++ show (_nassaYamlModuleType yaml)
      ]
    domainKeywords = maybe [] flattenDomainKeywords $ _nassaYamlDomainKeywords yaml
    implementationKeywords =
      map
        (show . _implementationLanguage)
        (_nassaYamlImplementations yaml)

flattenDomainKeywords :: DomainKeyword -> [String]
flattenDomainKeywords domain =
  maybe [] id (_keywordSubjects domain)
    ++ maybe [] id (_keywordRegions domain)
    ++ maybe [] id (_keywordPeriods domain)
