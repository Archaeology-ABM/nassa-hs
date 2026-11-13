module NASSA.CLI.Track where

import           NASSA.History
import NASSA.ReadYml

import System.Directory (doesFileExist)

data TrackOptions = TrackOptions {
      _trackBaseDir         :: FilePath
    , _trackHistoryFilePath :: FilePath
    }

runTrack :: TrackOptions -> IO ()
runTrack (TrackOptions baseDir histFilePath) = do
    allModules <- readNassaModuleCollection False baseDir
    histFileExists <- doesFileExist histFilePath
    currentState <- makeHistory histFilePath allModules
    -- oldState <- if histFileExists
    --     then readHistory inPath
    --     else 
    writeHistory histFilePath currentState