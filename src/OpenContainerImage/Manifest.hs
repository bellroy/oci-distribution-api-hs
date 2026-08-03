{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- Manifests
--
-- See https://specs.opencontainers.org/image-spec
module OpenContainerImage.Manifest
  ( ImageManifest (..),
    Descriptor (..),

    -- * Digests
    Digest (..),
    renderDigest,
    digestAlgorithm,
    digestEncoded,

    -- * Extra types
    ManifestName,
    ManifestReference,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (FromJSON (parseJSON), ToJSON (toEncoding, toJSON))
import Data.Attoparsec.Text qualified as TextParse
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Word (Word64)
import GHC.Generics (Generic, Generically (..))
import GHC.TypeLits (KnownNat, KnownSymbol, natVal, symbolVal)
import OpenContainerImage.Manifest.Annotation qualified as Annotation
import Type.Reflection (typeRep)

data ImageManifest = ImageManifest
  { schemaVersion :: Literal 2,
    mediaType :: Literal "application/vnd.oci.image.manifest.v1+json",
    artifactType :: Literal "application/vnd.unknown.artifact.v1",
    config :: Descriptor,
    layers :: Vector Descriptor,
    annotations :: Maybe (Map Annotation.Key Text)
  }
  deriving stock (Show, Generic)
  deriving (ToJSON, FromJSON) via (Generically ImageManifest)

data Descriptor = Descriptor
  { -- | Media type for whatever is referred to by this descriptor
    mediaType :: Text,
    digest :: Digest,
    size :: Word64,
    annotations :: Maybe (Map Annotation.Key Text)
  }
  deriving stock (Show, Generic)
  deriving (ToJSON, FromJSON) via (Generically Descriptor)

-- | Digests
-- See https://specs.opencontainers.org/image-spec/descriptor/?v=v1.1.1#digests
data Digest = Digest
  { algorithm :: Text,
    encoded :: Text,
    rendered :: Text
  }
  deriving stock (Generic, Show)

data DigestAlgorithmParseState = StartAlgorithm | InAlgorithm | AlgInvalidChar
  deriving stock (Eq)

digestAlgorithm :: Digest -> Text
digestAlgorithm Digest {algorithm} = algorithm

digestEncoded :: Digest -> Text
digestEncoded Digest {encoded} = encoded

renderDigest :: Digest -> Text
renderDigest Digest {rendered} = rendered

instance ToJSON Digest where
  toJSON = toJSON . renderDigest
  toEncoding = toEncoding . renderDigest

instance FromJSON Digest where
  parseJSON =
    JSON.withText "Digest" $ \rendered -> do
      (algorithm, encoded) <- TextParse.parseOnly parser rendered & either fail pure
      pure Digest {algorithm, encoded, rendered}
    where
      -- grammar:
      -- digest                ::= algorithm ":" encoded
      -- algorithm             ::= algorithm-component (algorithm-separator algorithm-component)*
      -- algorithm-component   ::= [a-z0-9]+
      -- algorithm-separator   ::= [+._-]
      -- encoded               ::= [a-zA-Z0-9=_-]+
      parser :: TextParse.Parser (Text, Text)
      parser = do
        (algorithm, InAlgorithm) <- TextParse.runScanner StartAlgorithm scanAlgorithm
        _sep <- TextParse.char ':'
        encoded <- (TextParse.takeWhile1 inEncodedClass <* TextParse.endOfInput) TextParse.<?> "encoded"
        pure (algorithm, encoded)

      scanAlgorithm :: DigestAlgorithmParseState -> Char -> Maybe DigestAlgorithmParseState
      scanAlgorithm state ch = case state of
        InAlgorithm | ch == ':' -> Nothing
        StartAlgorithm; InAlgorithm
          | inAlgorithmComponentClass ch -> Just InAlgorithm
        InAlgorithm
          | inAlgorithmSeparatorClass ch -> Just StartAlgorithm
        _ -> Just AlgInvalidChar

      inAlgorithmComponentClass, inAlgorithmSeparatorClass, inEncodedClass :: Char -> Bool
      inAlgorithmComponentClass = TextParse.inClass "a-z0-9"
      inAlgorithmSeparatorClass = TextParse.inClass "+._-"
      inEncodedClass = TextParse.inClass "a-zA-Z0-9=_-"

type ManifestName = Text

-- | Either a tag (common ones are "v1", "latest", etc.) or a particular reference
type ManifestReference = Text

-- | aeson helper type
--
-- Gives you a ToJSON/FromJSON with exactly one valid (string or natural) value
data Literal a = Literal

instance (KnownSymbol a) => Show (Literal a) where
  showsPrec p _ = showsPrec p (symbolVal @a Proxy)

instance (KnownNat a) => Show (Literal a) where
  showsPrec p _ = showsPrec p (natVal @a Proxy)

instance (KnownSymbol a) => ToJSON (Literal a) where
  toJSON _ = toJSON (symbolVal @a Proxy)
  toEncoding _ = toEncoding (symbolVal @a Proxy)

instance (KnownSymbol a) => FromJSON (Literal a) where
  parseJSON =
    JSON.withText
      (show (typeRep @(Literal a)))
      (\input -> if input == Text.pack (symbolVal @a Proxy) then pure Literal else fail $ "expected " ++ show (symbolVal @a Proxy))

instance (KnownNat a) => ToJSON (Literal a) where
  toJSON _ = toJSON (natVal @a Proxy)
  toEncoding _ = toEncoding (natVal @a Proxy)

instance (KnownNat a) => FromJSON (Literal a) where
  parseJSON =
    JSON.withScientific
      (show (typeRep @(Literal a)))
      (\input -> if input == fromInteger (natVal @a Proxy) then pure Literal else fail $ "expected " ++ show (natVal @a Proxy))
