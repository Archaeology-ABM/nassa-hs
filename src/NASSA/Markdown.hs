{-# LANGUAGE OverloadedStrings #-}

module NASSA.Markdown (getDoc, extractSection) where

import           CMark
import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

getDoc :: FilePath -> IO Node
getDoc path = do
    docText <- T.decodeUtf8Lenient <$> B.readFile path
    return $ commonmarkToNode [] docText

extractSectionNodes :: Int -> T.Text -> Node -> [Node]
extractSectionNodes level header (Node _ DOCUMENT nodesAll) = go False nodesAll
  where
    go _ [] = []
    go inSection ((Node _ (HEADING l) [Node _ (TEXT t) _]):nodes)
      | inSection && l <= level = []
      | t == header && l == level = go True nodes
      | otherwise = go inSection nodes
    go inSection (node:nodes)
      | inSection = node : go inSection nodes
      | otherwise = go inSection nodes
extractSectionNodes _ _ _ = []

extractSection :: Int -> T.Text -> Node -> T.Text
extractSection level header doc = T.concat $ map (nodeToCommonmark [optSafe] Nothing) $ extractSectionNodes level header doc
