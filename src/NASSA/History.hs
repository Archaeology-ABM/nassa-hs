{-# LANGUAGE OverloadedStrings #-}

module NASSA.History where

import NASSA.Types
import NASSA.Utils

import           Control.Monad.IO.Class    (liftIO)
import           Data.Aeson                (FromJSON, ToJSON, object, parseJSON,
                                            toJSON, withObject, (.!=), (.:),
                                            (.:?), (.=))
import qualified Data.ByteString           as B
import           Data.Function             (on)
import           Data.List                 (elemIndex)
import           Data.Maybe                (fromMaybe)
import qualified Data.Set                  as S
import           Data.Time                 (Day, UTCTime (..), getCurrentTime)
import           Data.Yaml                 (decodeEither')
import           Data.Yaml.Pretty          (defConfig, encodePretty,
                                            setConfCompare, setConfDropNull)
import           GitHash                   (getGitInfo, giHash)
import           System.Directory          (createDirectoryIfMissing,
                                            makeAbsolute)
import           System.FilePath           (makeRelative, takeDirectory)
import           Data.Version              (Version (..), makeVersion)
import Control.Exception                   (throwIO)

data NassaModuleHistory = NassaModuleHistory
    { histYamlID          :: String
    , histYamlDescription    :: Maybe String
    , histYamlHistoryVersion :: Version
    , histYamlLastModified   :: Day
    , histYamlModules        :: S.Set ModuleIteration
    }
    deriving (Show, Eq)

instance FromJSON NassaModuleHistory where
    parseJSON = withObject "NassaModuleHistory" $ \v -> NassaModuleHistory
        <$> v .:  "title"
        <*> v .:? "description"
        <*> v .:  "historyVersion"
        <*> v .:  "lastModified"
        <*> v .:? "modules" .!= S.empty

instance ToJSON NassaModuleHistory where
    toJSON x = object $ [
        "title"            .= histYamlID x,
        "description"      .= histYamlDescription x,
        "historyVersion" .= histYamlHistoryVersion x,
        "lastModified"     .= histYamlLastModified x] ++
        if not $ null (histYamlModules x) then ["modules" .= histYamlModules x] else []

-- | A data type to represent a module state
data ModuleIteration = ModuleIteration
    { modStateID   :: String   -- ^ the ID of the module
    , modStateVersion :: Version  -- ^ the version of the module
    , modStateCommit  :: String   -- ^ the hash of a relevant commit where a module can be accessed in this version
    , modStatePath    :: FilePath -- ^ the file path where the POSEIDON.yml file is stored
    }
    deriving (Show)

instance Eq ModuleIteration where
    (ModuleIteration t1 v1 _ _) == (ModuleIteration t2 v2 _ _) = (t1 == t2) && (v1 == v2)

instance Ord ModuleIteration where
    (ModuleIteration t1 v1 _ _) `compare` (ModuleIteration t2 v2 _ _) = (t1,v1) `compare` (t2,v2)

instance FromJSON ModuleIteration where
    parseJSON = withObject "modules" $ \v -> ModuleIteration
        <$> v .: "id"
        <*> v .: "version"
        <*> v .: "commit"
        <*> v .: "path"

instance ToJSON ModuleIteration where
    toJSON x = object [
          "id"   .= modStateID x
        , "version" .= modStateVersion x
        , "commit"  .= modStateCommit x
        , "path"    .= modStatePath x
        ]

getNewModules :: NassaModuleHistory -> NassaModuleHistory -> S.Set ModuleIteration
getNewModules oldHistory newHistory =
    let oldModuleSet = histYamlModules oldHistory
        newModuleSet = histYamlModules newHistory
    in S.difference oldModuleSet newModuleSet

updateHistory :: NassaModuleHistory -> NassaModuleHistory -> NassaModuleHistory
updateHistory oldHistory newHistory =
    let oldModuleSet = histYamlModules oldHistory
        newModuleSet = histYamlModules newHistory
        mergedPacSet  = S.union oldModuleSet newModuleSet
        oldHistoryVersion = histYamlHistoryVersion oldHistory
    in
        if mergedPacSet == oldModuleSet
        then oldHistory
        else NassaModuleHistory {
              histYamlID            = histYamlID oldHistory
            , histYamlDescription      = histYamlDescription oldHistory
            , histYamlHistoryVersion = updateThreeComponentVersion Minor oldHistoryVersion
            , histYamlLastModified     = histYamlLastModified newHistory
            , histYamlModules          = mergedPacSet
            }

data VersionComponent = Major
    | Minor
    | Patch
    deriving Show

updateThreeComponentVersion :: VersionComponent -> Version -> Version
updateThreeComponentVersion component v =
    let i = versionBranch v
        r = case component of
            Patch -> [ i !! 0,      i !! 1,     (i !! 2) + 1]
            Minor -> [ i !! 0,     (i !! 1) + 1, 0          ]
            Major -> [(i !! 0) + 1,              0, 0       ]
    in makeVersion r

readHistory :: FilePath -> IO NassaModuleHistory
readHistory p = do
    bs <- liftIO $ B.readFile p
    case decodeEither' bs of
        Left err   -> throwIO $ NassaHistoryException (show err)
        Right hist -> return hist

writeHistory :: FilePath -> NassaModuleHistory -> IO ()
writeHistory p histShot = do
    liftIO $ createDirectoryIfMissing True $ takeDirectory p
    liftIO $ B.writeFile p (encodePretty opts histShot)
    where
        opts = setConfDropNull True $ setConfCompare (compare `on` fieldIndex) defConfig
        fieldIndex s = fromMaybe (length fields) $ s `elemIndex` fields
        fields = [
            "title",
            "description",
            "historyVersion",
            "lastModified",
            "modules",
            "id",
            "version",
            "commit",
            "path"
         ]

-- could be entirely avoided with a redesign
emptyHistory :: IO NassaModuleHistory
emptyHistory = do
    (UTCTime today _) <- liftIO getCurrentTime
    return $ NassaModuleHistory {
      histYamlID             = ""
    , histYamlDescription    = Just ""
    , histYamlHistoryVersion = makeVersion [0, 1, 0]
    , histYamlLastModified   = today
    , histYamlModules        = S.empty
    }

makeHistory :: FilePath -> [NassaModule] -> IO NassaModuleHistory
makeHistory pathToHistoryFile mods = do
    modHistorys <- historyModules pathToHistoryFile mods
    (UTCTime today _) <- liftIO getCurrentTime
    return $ NassaModuleHistory {
      histYamlID            = "History title"
    , histYamlDescription      = Just "History description"
    , histYamlHistoryVersion = makeVersion [0, 1, 0]
    , histYamlLastModified     = today
    , histYamlModules         = modHistorys
    }

historyModules :: FilePath -> [NassaModule] -> IO (S.Set ModuleIteration)
historyModules pathToHistoryFile mods = do
    modStateList <- mapM histOne mods
    return $ S.fromList modStateList
    where
        histOne :: NassaModule -> IO ModuleIteration
        histOne (NassaModule baseDir yaml) = do
            let version = _nassaYamlModuleVersion yaml
            commit <- getGitCommitHash baseDir
            return $ ModuleIteration {
                modStateID   = (\(ModuleID s) -> s) $ _nassaYamlID yaml,
                modStateVersion = version,
                modStateCommit  = commit,
                modStatePath    = makeRelative (takeDirectory pathToHistoryFile) baseDir
            }

getGitCommitHash :: FilePath -> IO String
getGitCommitHash p = do
    eitherGit <- liftIO $ getGitInfo p
    case eitherGit of
        Left _ -> do
            pAbsolute <- liftIO $ makeAbsolute p
            let oneLevelUp = takeDirectory pAbsolute
            if oneLevelUp == takeDirectory oneLevelUp
            then do return "No Git repository found"
            else getGitCommitHash oneLevelUp
        Right info -> do
            return $ giHash info
