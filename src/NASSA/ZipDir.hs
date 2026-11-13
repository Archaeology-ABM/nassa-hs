{-# LANGUAGE OverloadedStrings #-}

module NASSA.ZipDir where

import Codec.Archive.Zip
import Control.Monad (foldM)
import qualified Data.ByteString.Lazy as BL
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory, createTempDirectory)

zipDirectoryToTemp :: FilePath -> IO FilePath
zipDirectoryToTemp sourceDir = do
  systemTmp <- getTemporaryDirectory
  tmpDir <- createTempDirectory systemTmp "nassa"
  let outputZip = tmpDir </> (takeBaseName sourceDir ++ ".zip")
  files <- findFilesInDir sourceDir
  archive <- foldM (addFile sourceDir) emptyArchive files
  BL.writeFile outputZip (fromArchive archive)
  return outputZip

-- zipDirectoryToTemp :: FilePath -> IO FilePath
-- zipDirectoryToTemp sourceDir =
--   withSystemTempDirectory "nassa" $ \tmpDir -> do
--     let outputZip = tmpDir </> (takeBaseName sourceDir ++ ".zip")
--     putStrLn $ "Zipping " <> sourceDir
--     files <- findFilesInDir sourceDir
--     archive <- foldM (addFile sourceDir) emptyArchive files
--     BL.writeFile outputZip (fromArchive archive)
--     putStrLn $ "ZIP archive written to: " <> outputZip
--     return outputZip

addFile :: FilePath -> Archive -> FilePath -> IO Archive
addFile root archive file = do
  contents <- BL.readFile file
  modified <- getModificationTime file
  let zipPath =
        map (\c -> if isPathSeparator c then '/' else c) $
          makeRelative root file
      entry = toEntry zipPath
                      (floor $ utcTimeToPOSIXSeconds modified)
                      contents
  pure $ addEntryToArchive entry archive

findFilesInDir :: FilePath -> IO [FilePath]
findFilesInDir dir = do
  names <- listDirectory dir
  paths <- traverse (\name -> pure (dir </> name)) names
  fmap concat $ traverse go paths
  where
    go path = do
      isDir <- doesDirectoryExist path
      if isDir
        then findFilesInDir path
        else pure [path]
