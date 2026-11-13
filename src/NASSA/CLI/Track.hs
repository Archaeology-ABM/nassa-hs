module NASSA.CLI.Track where

import           NASSA.History
import NASSA.ReadYml
import NASSA.ZipDir

import System.Directory (doesFileExist)
import System.FilePath ((</>))

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
    putStrLn $ show diff
    
    -- using baseDir is problematic as things currently stand; the paths may be in relation to the histFile
    mapM_ (zipModule baseDir) diff
   
    -- let newState = updateHistory oldState currentState
    -- writeHistory histFilePath newState
    
    
zipModule :: FilePath -> ModuleIteration -> IO FilePath
zipModule baseDir (ModuleIteration _ _ _ path) = zipDirectoryToTemp (baseDir </> path)
    
