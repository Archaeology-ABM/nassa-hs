{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module NASSA.SPDXLicense where

import           Control.Monad                        (mzero)
import           Data.Aeson
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Lazy                 as BL
import qualified Data.FileEmbed                       as FE
import           Data.List                            (elemIndex)
import           Data.Maybe                           (fromJust)
import qualified Data.Text                            as T
import           Options.Applicative.Help.Levenshtein (editDistance)


licensesJSONFileBS :: ByteString
licensesJSONFileBS = $(FE.embedFile "data/licenses.json")

data LicensesJSON = LicensesJSON {
    _ljLicenseListVersion :: T.Text,
    _ljLicenses           :: [License]
}

instance FromJSON LicensesJSON where
    parseJSON = withObject "LicensesJSON" $ \v -> LicensesJSON
        <$> v .: "licenseListVersion"
        <*> v .: "licenses"

data License = License {
    _lName :: T.Text,
    _lID   :: T.Text
}

instance FromJSON License where
    parseJSON = withObject "License" $ \v -> License
        <$> v .: "name"
        <*> v .: "licenseId"

validLicenseIDs :: [T.Text]
validLicenseIDs =
    case eitherDecode (BL.fromStrict licensesJSONFileBS) of
        Left _                          -> []
        Right (LicensesJSON _ licenses) -> map _lID licenses

validLicenseIDsString :: [String]
validLicenseIDsString = map T.unpack validLicenseIDs

newtype SPDXLicense = SPDXLicense T.Text deriving (Show, Eq)

instance FromJSON SPDXLicense where
    parseJSON (String s) =
        if s `elem` validLicenseIDs
        then pure $ SPDXLicense s
        else fail $ show s ++ " not a valid SPDX License (https://spdx.org/licenses, Version 3.25.0, 2024-08-19); " ++
                    "maybe you meant " ++ show (findSimilar validLicenseIDsString (T.unpack s))
    parseJSON _ = mzero

findSimilar ::  [String] -> String -> String
findSimilar [] _  = []
findSimilar ref x =
    let dists = map (\y -> x `editDistance` y) ref
    in ref !! fromJust (elemIndex (minimum dists) dists)
