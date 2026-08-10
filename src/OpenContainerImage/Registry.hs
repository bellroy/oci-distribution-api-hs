{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NoFieldSelectors #-}
-- bad
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module OpenContainerImage.Registry
  ( -- * Basic registry client
    RegistryClient,
    newClient,

    -- ** Debugging
    withResponsePrinting,

    -- * Endpoint implementations

    -- ** getImageManifest
    GetImageManifestError,
    getImageManifest,

    -- ** withBlobFromDigest
    WithBlobFromDigestError,
    withBlobFromDigest,
    consumeBody,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (guard, void, (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont
import Data.Aeson (FromJSON)
import Data.Aeson qualified as JSON
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (foldrM)
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word64)
import GHC.Generics (Generic, Generically (..))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (hWWWAuthenticate)
import Network.HTTP.Types.Status qualified as HTTP
import OpenContainerImage.Manifest
  ( Digest,
    ImageManifest,
    ManifestName,
    ManifestReference,
    renderDigest,
  )
import Text.URI (Authority (..), URI (..), UserInfo (..))
import Text.URI qualified as URI
import UnliftIO.IORef

data RegistryClient = RegistryClient
  { host :: ByteString,
    secure :: Bool,
    port :: Int,
    auth :: RegistryAuth,
    logResponse :: HTTP.Response () -> IO () {- does not have access to the response body -},
    manager :: HTTP.Manager
  }

data RegistryAuth
  = NoAuth
  | NeedAuth
      { basicAuthUsername :: ByteString,
        basicAuthPassword :: ByteString,
        applyAuthRef :: IORef (HTTP.Request -> HTTP.Request)
      }

{-# INLINE newClient #-}
newClient :: Text -> HTTP.Manager -> IO (Maybe RegistryClient)
newClient baseUri manager = mapM provideAuthCfg $ do
  URI
    { uriScheme = Just scheme,
      uriAuthority =
        Right
          Authority
            { authUserInfo = basicAuth,
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
    ( basicAuth,
      \auth ->
        RegistryClient
          { host = Text.encodeUtf8 (URI.unRText authHost),
            secure = URI.unRText scheme == "https",
            port,
            auth,
            manager,
            logResponse = \_ -> pure ()
          }
    )
  where
    provideAuthCfg :: (Maybe URI.UserInfo, RegistryAuth -> RegistryClient) -> IO RegistryClient
    provideAuthCfg (basicAuthMaybe, mkClient) = case basicAuthMaybe of
      Nothing -> pure $! mkClient NoAuth
      Just UserInfo {uiUsername, uiPassword} ->
        newIORef id <&> \applyAuthRef ->
          mkClient
            NeedAuth
              { basicAuthUsername = Text.encodeUtf8 (URI.unRText uiUsername),
                basicAuthPassword = maybe "" (Text.encodeUtf8 . URI.unRText) uiPassword,
                applyAuthRef
              }

-- | Useful for debugging
withResponsePrinting :: RegistryClient -> RegistryClient
withResponsePrinting client =
  client
    { logResponse = print
    }

--------------------------------------------------------------------------------
-- Endpoints

data RegistryError endpointError
  = RegistryAuthFail Text
  | RegistryError endpointError
  deriving stock (Show)

data GetImageManifestError
  = GetImageManifestError'ManifestNotFound
  | GetImageManifestError'ManifestParseError String
  | GetImageManifestError'OtherResponseError HTTP.Status
  deriving stock (Show)

-- | end-3 GET /v2/<name>/manifests/<reference>
getImageManifest ::
  RegistryClient ->
  ManifestName ->
  ManifestReference ->
  IO (Either (RegistryError GetImageManifestError) ImageManifest)
getImageManifest client name reference = do
  doRegistryRequest
    client
    ( \request ->
        request
          { HTTP.method = "GET",
            HTTP.path = Text.encodeUtf8 (Text.concat ["/v2/", name, "/manifests/", reference]),
            HTTP.requestHeaders = [("Accept", "application/vnd.oci.image.manifest.v1+json")]
          }
    )
    ( \response ->
        case HTTP.statusCode (HTTP.responseStatus response) of
          200 -> do
            body <- consumeBody (HTTP.responseBody response)
            pure $! first GetImageManifestError'ManifestParseError (JSON.eitherDecode body)
          404 -> pure (Left GetImageManifestError'ManifestNotFound)
          _ -> pure (Left (GetImageManifestError'OtherResponseError (HTTP.responseStatus response)))
    )

data WithBlobFromDigestError
  = WithBlobFromDigestError'BlobNotFound
  | WithBlobFromDigestError'OtherError HTTP.Status
  deriving stock (Show)

-- | end-2 GET /v2/<name>/blob/<digest>
--
-- Since what is in these digests may be whatever you want, the caller is given
-- complete control over how it consumes the blob.
--
-- The handler you pass to this function must not "leak" the HTTP.BodyReader to
-- the outer scope via the @Either e a@ result. Doing so will mean the
-- HTTP.BodyReader surviving beyond the lifetime of the connection.
withBlobFromDigest ::
  RegistryClient ->
  ManifestName ->
  Digest ->
  (HTTP.BodyReader -> IO a) ->
  IO (Either (RegistryError WithBlobFromDigestError) a)
withBlobFromDigest client name digest withDigestBody =
  doRegistryRequest
    client
    ( \request ->
        request
          { HTTP.path = Text.encodeUtf8 (Text.concat ["/v2/", name, "/blobs/", renderDigest digest])
          }
    )
    ( \response -> case HTTP.statusCode (HTTP.responseStatus response) of
        200 -> Right <$> withDigestBody (HTTP.responseBody response)
        404 -> pure (Left WithBlobFromDigestError'BlobNotFound)
        _ -> pure (Left (WithBlobFromDigestError'OtherError (HTTP.responseStatus response)))
    )

consumeBody :: HTTP.BodyReader -> IO BL.ByteString
consumeBody = fmap BL.fromChunks . HTTP.brConsume

--------------------------------------------------------------------------------
-- Implementation internals

-- | Handles auth flow as per
-- https://distribution.github.io/distribution/spec/auth/token/
--
-- Every request to a registry must be via this function in order to validly
-- perform the auth flow
doRegistryRequest ::
  RegistryClient ->
  (HTTP.Request -> HTTP.Request) ->
  (HTTP.Response HTTP.BodyReader -> IO (Either e a)) ->
  IO (Either (RegistryError e) a)
doRegistryRequest RegistryClient {secure, host, port, auth, manager, logResponse} modifyBaseRequest handleResponse =
  evalContT $
    -- For prior art that is fairly easy to follow on this, see:
    -- https://github.com/cloverzero/peeko/blob/984b4aa594416c63b35d44f053d1b290130c1805/peeko/src/registry/client.rs#L141
    case auth of
      -- Attempt request with no auth flow. Probably can't work for most
      -- registries but provided anyway
      NoAuth -> do
        response <- HTTP.withResponse request manager & ContT
        lift (logResponse (void response))
        case HTTP.statusCode (HTTP.responseStatus response) of
          401 -> pure (Left (RegistryAuthFail "unauthorised and no auth method available"))
          _ -> lift (handleResponse response <&> first RegistryError)
      -- Attempt request with token auth flow as per https://distribution.github.io/distribution/spec/auth/token/
      -- i.e.
      -- 1. Make a request
      -- 2. If that 401s, try get the "Bearer <BEARER>" out of WWW-Authenticate in the response
      -- 3. If that was successful, use <BEARER> to request an auth token
      -- 4. If that was successful, use that auth token to make the request in (1) again
      --
      -- Token expiry is provided by (3) but we don't use it. We'll get a 401
      -- anyway when attempting to use an expired token, which will then trigger
      -- the auth flow.
      NeedAuth {basicAuthUsername, basicAuthPassword, applyAuthRef} -> do
        let doRegistryRequestNewAuth newApplyAuth = do
              writeIORef applyAuthRef newApplyAuth
              tryDoRegistryRequestWithAuth newApplyAuth
            tryDoRegistryRequestWithAuth applyAuth = do
              response <- HTTP.withResponse (applyAuth request) manager & ContT
              lift (logResponse (void response))
              pure response
        response <- tryDoRegistryRequestWithAuth =<< readIORef applyAuthRef
        case HTTP.statusCode (HTTP.responseStatus response) of
          401
            | Just (authMode, authParams) <- getRegistryAuthParams response -> do
                HTTP.responseClose response & lift
                case authMode of
                  RegistryAuthBasic -> do
                    response <- doRegistryRequestNewAuth (HTTP.applyBasicAuth basicAuthUsername basicAuthPassword)
                    case HTTP.statusCode (HTTP.responseStatus response) of
                      401 -> pure (Left (RegistryAuthFail "still unauthorised after basic auth token flow"))
                      _ -> handleResponse response <&> first RegistryError & lift
                  RegistryAuthBearer -> do
                    newToken <- doRegistryAuthParamsRequest manager authParams basicAuthUsername basicAuthPassword & lift
                    case newToken of
                      Just token -> do
                        response <- doRegistryRequestNewAuth (HTTP.applyBearerAuth token)
                        case HTTP.statusCode (HTTP.responseStatus response) of
                          401 -> pure (Left (RegistryAuthFail "still unauthorised after bearer auth token flow"))
                          _ -> handleResponse response <&> first RegistryError & lift
                      Nothing ->
                        pure (Left (RegistryAuthFail "registry did not supply auth token after auth token flow"))
            | otherwise ->
                pure (Left (RegistryAuthFail "unauthorised and registry did not begin auth token flow"))
          _ ->
            handleResponse response <&> first RegistryError & lift
  where
    request =
      HTTP.defaultRequest
        { HTTP.secure = secure,
          HTTP.port = port,
          HTTP.host = host,
          -- See https://github.com/opencontainers/distribution-spec/blob/main/spec.md#api
          -- "... clients ... MUST NOT forward Authorization headers across host
          -- boundaries unless explicitly configured to do so."
          HTTP.shouldStripHeaderOnRedirect = \case
            "Authorization" -> True
            headerName -> HTTP.shouldStripHeaderOnRedirect HTTP.defaultRequest headerName
        }
        & modifyBaseRequest

data RegistryAuthMode
  = RegistryAuthBearer
  | RegistryAuthBasic
  deriving stock (Show)

data RegistryAuthParams = RegistryAuthParams
  {realm, service, scope :: Text}
  deriving stock (Show)

-- | Parse the Bearer info from a HTTP response's WWW-Authenticate header, if
-- supplied
getRegistryAuthParams :: HTTP.Response r -> Maybe (RegistryAuthMode, RegistryAuthParams)
getRegistryAuthParams = List.lookup hWWWAuthenticate . HTTP.responseHeaders >=> parseRegistryAuthParams

parseRegistryAuthParams :: ByteString -> Maybe (RegistryAuthMode, RegistryAuthParams)
parseRegistryAuthParams bs = do
  text <- Text.decodeASCII' bs
  (mode, content) <-
    if
      | Just bearer <- Text.stripPrefix "Bearer " text -> Just (RegistryAuthBearer, bearer)
      | Just basic <- Text.stripPrefix "Basic " text -> Just (RegistryAuthBasic, basic)
      | otherwise -> Nothing
  params <- parse content >>= ensureNonEmpty
  pure (mode, params)
  where
    parse :: Text -> Maybe RegistryAuthParams
    parse params =
      foldrM
        parseAddAuthParam
        (RegistryAuthParams "" "" "")
        (map Text.strip (Text.split (== ',') params))

    parseAddAuthParam :: Text -> RegistryAuthParams -> Maybe RegistryAuthParams
    parseAddAuthParam param info@RegistryAuthParams {realm, service, scope} =
      case key of
        "realm" | Text.null realm -> parseValue value <&> \v -> info {realm = v}
        "service" | Text.null service -> parseValue value <&> \v -> info {service = v}
        "scope" | Text.null scope -> parseValue value <&> \v -> info {scope = v}
        _ -> Just info -- ignore unknown keys
      where
        (key, value) = Text.break (== '=') param

    -- technically not spec-compliant with RFC 7235 but every registry does use quoted strings
    parseValue :: Text -> Maybe Text
    parseValue = Text.stripPrefix "=\"" >=> Text.stripSuffix "\""

    ensureNonEmpty :: RegistryAuthParams -> Maybe RegistryAuthParams
    ensureNonEmpty info@RegistryAuthParams {realm, service} =
      info <$ guard (not (Text.null realm || Text.null service))

data RegistryTokenResponse = RegistryTokenResponse
  { token :: Maybe Text,
    access_token :: Maybe Text,
    expires_in :: Maybe Word64,
    refresh_token :: Maybe Text
  }
  deriving stock (Generic)
  deriving (FromJSON) via (Generically RegistryTokenResponse)

-- | Make HTTP request according to the 'RegistryAuthParams' (i.e. the
-- WWW-Authenticate value)
doRegistryAuthParamsRequest ::
  HTTP.Manager ->
  RegistryAuthParams ->
  -- | username
  ByteString ->
  -- | password
  ByteString ->
  IO (Maybe ByteString)
doRegistryAuthParamsRequest manager RegistryAuthParams {realm, service, scope} username password = do
  response <- HTTP.httpLbs request manager
  case HTTP.statusCode (HTTP.responseStatus response) of
    200 ->
      pure $! do
        -- token ior access_token are provided (and not neither)
        RegistryTokenResponse {token, access_token} <- JSON.decode (HTTP.responseBody response)
        Text.encodeUtf8 <$> (token <|> access_token)
    _ -> pure Nothing
  where
    request =
      (HTTP.parseRequest_ (Text.unpack realm))
        { HTTP.requestHeaders = [("Accept", "application/json")],
          HTTP.shouldStripHeaderOnRedirect = \case
            "Authorization" -> True
            headerName -> HTTP.shouldStripHeaderOnRedirect HTTP.defaultRequest headerName
        }
        & HTTP.setQueryString query
        & HTTP.applyBasicAuth username password
    query =
      ("service", Just (Text.encodeUtf8 service))
        : [("scope", Just (Text.encodeUtf8 scope)) | not (Text.null scope)]
