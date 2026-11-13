{-# LANGUAGE OverloadedStrings #-}

module NASSA.CLI.Track where

import           NASSA.History
import NASSA.ReadYml
import NASSA.ZipDir
import NASSA.Zenodo
import NASSA.Types

import           Data.Time                 (Day, UTCTime (..), getCurrentTime)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import qualified Data.Set as S

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
   
    -- let newState = updateHistory oldState currentState
    -- writeHistory histFilePath newState
    
depositModuleAtZenodo :: FilePath -> NassaModule -> IO ()
depositModuleAtZenodo baseDir (NassaModule moduleDir yaml) = do
  -- zip
  zipArchivePath <- zipDirectoryToTemp (baseDir </> moduleDir)
  -- prep submission
  (UTCTime today _) <- getCurrentTime
  let token = "YOUR_SANDBOX_ACCESS_TOKEN"
      metadata =
        Metadata
          { metadataTitle = "Example research dataset"
          , metadataDescription =
              "<p>An example dataset uploaded through the Zenodo API.</p>"
          , metadataPublicationDate = today
          , metadataCreators =
              [ Creator
                  { creatorName = "Doe, Jane"
                  , creatorAffiliation = Just "Example University"
                  , creatorOrcid = Just "0000-0002-1694-233X"
                  }
              ]
          , metadataUploadType = "dataset"
          , metadataPublicationType = Nothing
          , metadataAccessRight = "open"
          , metadataLicense = Just "cc-by-4.0"
          , metadataKeywords =
              ["example", "Haskell", "Zenodo"]
          , metadataVersion = Just "1.0.0"
          }
  result <- depositZip (sandboxClient token) metadata zipArchivePath
  print result
  
