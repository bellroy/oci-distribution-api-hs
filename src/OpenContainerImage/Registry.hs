{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module OpenContainerImage.Registry
  ( -- * Basic registry client
    RegistryClient (..),
    newClient,
    withModifyRequest,

    -- * Endpoint implementations
    GetImageManifestError,
    getImageManifest,
    BlobReader,
    GetBlobFromDigestError,
    getBlobFromDigest,
  )
where

import Control.Monad (guard, (>=>))
import Data.Aeson qualified as JSON
import Data.Bifunctor (first)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (status200, status404)
import OpenContainerImage.Manifest
  ( Digest,
    ImageManifest,
    ManifestName,
    ManifestReference,
    renderDigest,
  )
import Text.URI (Authority (..), URI (..), UserInfo (..))
import Text.URI qualified as URI

data RegistryClient = RegistryClient
  { host :: B.ByteString,
    secure :: Bool,
    port :: Int,
    modifyRequest :: HTTP.Request -> HTTP.Request,
    manager :: HTTP.Manager
  }

{-# INLINE newClient #-}
newClient :: Text -> HTTP.Manager -> Maybe RegistryClient
newClient baseUri manager = do
  URI
    { uriScheme = Just scheme,
      uriAuthority =
        Right
          Authority
            { authUserInfo,
              authHost,
              authPort
            },
      uriPath = Nothing,
      uriQuery = [],
      uriFragment = Nothing
    } <-
    URI.mkURI baseUri
  port <- case authPort of
    Just port ->
      guard (port <= (fromIntegral (maxBound :: Int) :: Word))
        $> fromIntegral port
    Nothing -> case URI.unRText scheme of
      "http" -> Just 80
      "https" -> Just 443
      _ -> Nothing
  Just
    RegistryClient
      { host = Text.encodeUtf8 (URI.unRText authHost),
        secure = URI.unRText scheme == "https",
        port,
        modifyRequest =
          case authUserInfo of
            Nothing -> id
            Just UserInfo {uiUsername, uiPassword} ->
              HTTP.applyBasicAuth
                (Text.encodeUtf8 (URI.unRText uiUsername))
                (maybe "" (Text.encodeUtf8 . URI.unRText) uiPassword),
        manager
      }

{-# INLINE withModifyRequest #-}
withModifyRequest :: (HTTP.Request -> HTTP.Request) -> RegistryClient -> RegistryClient
withModifyRequest fn client@RegistryClient {modifyRequest} = client {modifyRequest = fn . modifyRequest}

--------------------------------------------------------------------------------
-- Endpoint implementations

data GetImageManifestError
  = GetImageManifestError'ManifestNotFound
  | GetImageManifestError'ManifestParseError String
  | GetImageManifestError'OtherResponseError (HTTP.Response BL.ByteString)
  deriving stock (Show)

-- | end-3 GET /v2/<name>/manifests/<reference>
getImageManifest ::
  RegistryClient ->
  ManifestName ->
  ManifestReference ->
  IO (Either GetImageManifestError ImageManifest)
getImageManifest RegistryClient {host, secure, port, modifyRequest, manager} name reference = do
  response <- HTTP.httpLbs request manager -- this does _not_ perform lazy IO
  pure $! case HTTP.responseStatus response of
    status
      | status == status200 -> first GetImageManifestError'ManifestParseError (JSON.eitherDecode (HTTP.responseBody response))
      | status == status404 -> Left GetImageManifestError'ManifestNotFound
      | otherwise -> Left (GetImageManifestError'OtherResponseError response)
  where
    request =
      HTTP.defaultRequest
        { HTTP.method = "GET",
          HTTP.secure = secure,
          HTTP.port = port,
          HTTP.host = host,
          HTTP.path = Text.encodeUtf8 (Text.concat ["/v2/", name, "/manifests/", reference]),
          HTTP.requestHeaders = [("Accept", "application/vnd.oci.image.manifest.v1+json")]
        }
        & modifyRequest

type BlobReader a = HTTP.BodyReader -> IO a

data GetBlobFromDigestError
  = GetBlobFromDigestError'BlobNotFound
  | GetBlobFromDigestError'OtherResponseError (HTTP.Response BL.ByteString)
  deriving stock (Show)

-- | end-2 GET /v2/<name>/blob/<digest>
getBlobFromDigest ::
  RegistryClient ->
  ManifestName ->
  Digest ->
  (Either GetBlobFromDigestError HTTP.BodyReader -> IO a) ->
  IO a
getBlobFromDigest RegistryClient {host, secure, port, modifyRequest, manager} name digest withResult =
  HTTP.withResponse request manager (handleResponse >=> withResult)
  where
    request =
      HTTP.defaultRequest
        { HTTP.method = "GET",
          HTTP.secure = secure,
          HTTP.port = port,
          HTTP.host = host,
          HTTP.path = Text.encodeUtf8 (Text.concat ["/v2/", name, "/blobs/", renderDigest digest])
        }
        & modifyRequest
    handleResponse resp =
      case HTTP.responseStatus resp of
        status
          | status == status200 -> pure (Right (HTTP.responseBody resp))
          | status == status404 -> pure (Left GetBlobFromDigestError'BlobNotFound)
          | otherwise -> Left . GetBlobFromDigestError'OtherResponseError <$> mapM (fmap BL.fromChunks . HTTP.brConsume) resp
