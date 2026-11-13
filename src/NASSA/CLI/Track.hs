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
    oldState <- if histFileExists
         then readHistory histFilePath
         else emptyHistory
    currentState <- makeHistory histFilePath allModules
    let diff = getNewModules oldState currentState
    putStrLn $ show diff
    let newState = updateHistory oldState currentState
    writeHistory histFilePath newState