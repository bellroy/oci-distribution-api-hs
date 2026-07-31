{-# LANGUAGE OverloadedStrings #-}

module OpenContainerImage.Registry
  ( -- * Basic registry client
    getImageManifest,
    getBlobFromDigest,

    -- * Endpoint implementations
  )
where

import Data.Aeson qualified as JSON
import Data.ByteString qualified as B
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Header qualified as HTTP
import OpenContainerImage.Manifest

data RegistryClient = RegistryClient
  { host :: B.ByteString,
    headers :: HTTP.RequestHeaders,
    manager :: HTTP.Manager
  }

--------------------------------------------------------------------------------
-- end-3 GET /v2/<name>/manifests/<reference>

newtype ManifestName = ManifestName Text

newtype ManifestReference = ManifestReference Text

getImageManifest ::
  RegistryClient ->
  ManifestName ->
  ManifestReference ->
  IO (Either String ImageManifest)
getImageManifest RegistryClient {host, headers, manager} (ManifestName name) (ManifestReference reference) = do
  HTTP.httpLbs request manager -- note this does _not_ perform lazy IO
    <&> JSON.eitherDecode . HTTP.responseBody
  where
    request =
      HTTP.defaultRequest
        { HTTP.method = "GET",
          HTTP.secure = True,
          HTTP.port = 443,
          HTTP.host = host,
          HTTP.path = Text.encodeUtf8 (Text.concat ["/v2/", name, "/manifests/", reference]),
          HTTP.requestHeaders =
            ("Accept", "application/vnd.docker.distribution.manifest.v2+json")
              : headers
        }

--------------------------------------------------------------------------------
-- end-2 GET /v2/<name>/blob/<digest>

getBlobFromDigest :: RegistryClient -> ManifestName -> Digest -> (HTTP.Response HTTP.BodyReader -> IO a) -> IO a
getBlobFromDigest RegistryClient {host, headers, manager} (ManifestName name) digest =
  HTTP.withResponse request manager
  where
    request =
      HTTP.defaultRequest
        { HTTP.method = "GET",
          HTTP.secure = True,
          HTTP.port = 443,
          HTTP.host = host,
          HTTP.path = Text.encodeUtf8 (Text.concat ["/v2/", name, "/blob/", renderDigest digest]),
          HTTP.requestHeaders =
            ("Accept", "application/vnd.docker.distribution.manifest.v2+json")
              : headers
        }
