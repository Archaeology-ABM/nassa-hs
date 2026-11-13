{-# LANGUAGE OverloadedStrings #-}

module NASSA.Zenodo
  ( ZenodoClient(..)
  , Metadata(..)
  , Creator(..)
  , DepositResult(..)
  , productionClient
  , sandboxClient
  , depositZip
  ) where

import Control.Monad (unless, void)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (urlEncode)
import System.FilePath (takeExtension, takeFileName)
import System.IO ( IOMode(ReadMode), hFileSize, withBinaryFile)

-- | Zenodo connection information.
--
-- Do not include a trailing slash in 'zcBaseUrl'.
data ZenodoClient = ZenodoClient
  { zcBaseUrl   :: String
  , zcToken     :: Text
  , zcUserAgent :: BS.ByteString
  } deriving (Show)

productionClient :: Text -> ZenodoClient
productionClient token =
  ZenodoClient
    { zcBaseUrl = "https://zenodo.org"
    , zcToken = token
    , zcUserAgent = "my-zenodo-uploader/1.0"
    }

sandboxClient :: Text -> ZenodoClient
sandboxClient token =
  ZenodoClient
    { zcBaseUrl = "https://sandbox.zenodo.org"
    , zcToken = token
    , zcUserAgent = "my-zenodo-uploader/1.0"
    }

data Creator = Creator
  { creatorName        :: Text
  , creatorAffiliation :: Maybe Text
  , creatorOrcid       :: Maybe Text
  } deriving (Show)

instance ToJSON Creator where
  toJSON creator = object $
      [ "name" .= creatorName creator ]
      ++ catMaybes
        [ ("affiliation" .=) <$> creatorAffiliation creator
        , ("orcid" .=) <$> creatorOrcid creator
        ]

-- | Basic Zenodo metadata.
--
-- 'metadataUploadType' must be a Zenodo upload type such as:
-- "dataset", "software", "publication", "image", or "other".
--
-- If the upload type is "publication", set 'metadataPublicationType',
-- for example to "article", "report", or "preprint".
data Metadata = Metadata
  { metadataTitle           :: Text
  , metadataDescription     :: Text
  , metadataPublicationDate :: Day
  , metadataCreators        :: [Creator]
  , metadataUploadType      :: Text
  , metadataPublicationType :: Maybe Text
  , metadataAccessRight     :: Text
  , metadataLicense         :: Maybe Text
  , metadataKeywords        :: [Text]
  , metadataVersion         :: Maybe Text
  } deriving (Show)

instance ToJSON Metadata where
  toJSON metadata = object $
      [ "title" .= metadataTitle metadata
      , "description" .= metadataDescription metadata
      , "publication_date" .= formatDay (metadataPublicationDate metadata)
      , "creators" .= metadataCreators metadata
      , "upload_type" .= metadataUploadType metadata
      , "access_right" .= metadataAccessRight metadata
      ]
      ++ catMaybes
        [ ("publication_type" .=) <$> metadataPublicationType metadata
        , ("license" .=) <$> metadataLicense metadata
        , if null (metadataKeywords metadata)
            then Nothing
            else Just ("keywords" .= metadataKeywords metadata)
        , ("version" .=) <$> metadataVersion metadata
        ]

data DepositResult = DepositResult
  { resultDepositionId :: Integer
  , resultRecordId     :: Maybe Integer
  , resultDoi          :: Maybe Text
  , resultDoiUrl       :: Maybe Text
  } deriving (Show)

instance FromJSON DepositResult where
  parseJSON = withObject "DepositResult" $ \o ->
    DepositResult
      <$> o .:  "id"
      <*> o .:? "record_id"
      <*> o .:? "doi"
      <*> o .:? "doi_url"

data EmptyDeposit = EmptyDeposit
  { emptyDepositId     :: Integer
  , emptyDepositBucket :: String
  }

instance FromJSON EmptyDeposit where
  parseJSON = withObject "EmptyDeposit" $ \o -> do
    depositionId <- o .: "id"
    links <- o .: "links"
    bucket <- parseBucket links
    pure EmptyDeposit
      { emptyDepositId = depositionId
      , emptyDepositBucket = bucket
      }
    where
      parseBucket :: Value -> Parser String
      parseBucket = withObject "links" (.: "bucket")

-- | Create, upload, describe, and publish one ZIP archive.
--
-- The token requires at least the following scopes:
--
-- * deposit:write
-- * deposit:actions
--
-- Publishing is immediate. While testing, use 'sandboxClient'.
depositZip :: ZenodoClient -> Metadata -> FilePath -> IO DepositResult
depositZip client metadata zipPath = do
  validateZipPath zipPath
  manager <- newManager tlsManagerSettings
  -- 1. Create an empty deposition.
  created <- createDeposition manager client
  -- 2. Upload the ZIP archive.
  uploadFile manager client (emptyDepositBucket created) zipPath
  -- 3. Attach metadata.
  updateMetadata manager client (emptyDepositId created) metadata
  -- 4. Publish it.
  publishDeposition manager client (emptyDepositId created)

createDeposition :: Manager -> ZenodoClient -> IO EmptyDeposit
createDeposition manager client = do
  request0 <- parseRequest $ zcBaseUrl client ++ "/api/deposit/depositions"
  let request =
        request0
          { method = "POST"
          , requestHeaders = jsonHeaders client
          , requestBody = RequestBodyLBS "{}"
          }
  response <- httpLbs request manager
  decodeResponse "create deposition" response

uploadFile :: Manager -> ZenodoClient -> String -> FilePath -> IO ()
uploadFile manager client bucketUrl filePath = do
  body <- fileRequestBody filePath
  let encodedName = B8.unpack $ urlEncode True $ TE.encodeUtf8 $ pack $ takeFileName filePath
  request0 <- parseRequest (bucketUrl ++ "/" ++ encodedName)
  let request =
        request0
          { method = "PUT"
          , requestHeaders =
              authHeaders client
                ++ [(hContentType, "application/zip")]
          , requestBody = body
          }
  response <- httpLbs request manager
  ensureSuccess "upload file" response


updateMetadata :: Manager -> ZenodoClient -> Integer -> Metadata -> IO ()
updateMetadata manager client depositionId metadata = do
  request0 <- parseRequest $
    zcBaseUrl client
      ++ "/api/deposit/depositions/"
      ++ show depositionId
  let request =
        request0
          { method = "PUT"
          , requestHeaders = jsonHeaders client
          , requestBody =
              RequestBodyLBS $
                encode $
                  object ["metadata" .= metadata]
          }
  response <- httpLbs request manager
  ensureSuccess "update metadata" response

publishDeposition :: Manager -> ZenodoClient -> Integer -> IO DepositResult
publishDeposition manager client depositionId = do
  request0 <- parseRequest $
    zcBaseUrl client
      ++ "/api/deposit/depositions/"
      ++ show depositionId
      ++ "/actions/publish"
  let request =
        request0
          { method = "POST"
          , requestHeaders = authHeaders client
          , requestBody = RequestBodyLBS ""
          }
  response <- httpLbs request manager
  decodeResponse "publish deposition" response

authHeaders :: ZenodoClient -> RequestHeaders
authHeaders client =
  [ ( hAuthorization, "Bearer " <> TE.encodeUtf8 (zcToken client))
  , (hUserAgent, zcUserAgent client)
  ]

jsonHeaders :: ZenodoClient -> RequestHeaders
jsonHeaders client = authHeaders client  ++ [(hContentType, "application/json")]

-- | Construct a streaming request body, avoiding loading the complete ZIP
-- archive into memory.
fileRequestBody :: FilePath -> IO RequestBody
fileRequestBody path = do
  fileSize <- withBinaryFile path ReadMode hFileSize
  pure $
    RequestBodyStream
      (fromIntegral fileSize :: Int64)
      (\consumePopper ->
        withBinaryFile path ReadMode $ \handle ->
          consumePopper (BS.hGetSome handle (64 * 1024))
      )

decodeResponse :: FromJSON a => String -> Response LBS.ByteString -> IO a
decodeResponse operation response = do
  ensureSuccess operation response
  case eitherDecode (responseBody response) of
    Left err ->
      ioError $
        userError $
          operation
            ++ ": could not decode Zenodo response: "
            ++ err
    Right value ->
      pure value

ensureSuccess :: String -> Response LBS.ByteString -> IO ()
ensureSuccess operation response = do
  let code = statusCode (responseStatus response)
  unless (code >= 200 && code < 300) $
    ioError $
      userError $
        operation
          ++ " failed with HTTP "
          ++ show code
          ++ ": "
          ++ B8.unpack (LBS.toStrict (responseBody response))

validateZipPath :: FilePath -> IO ()
validateZipPath path =
  unless (map toLower (takeExtension path) == ".zip") $
    ioError $ userError "depositZip expects a file with a .zip extension"

formatDay :: Day -> String
formatDay = formatTime defaultTimeLocale "%F"
