{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | https://specs.opencontainers.org/image-spec/annotations/
module OpenContainerImage.Manifest.Annotation
  ( Key,
    custom,
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Text (Text)
import GHC.OverloadedLabels (IsLabel (..))

-- | An annotation key
--
-- Pre-defined keys in the list at
-- https://specs.opencontainers.org/image-spec/annotations/ are given an
-- 'IsLabel' instance, so can be referred to syntax like
-- #"org.opencontainers.image.created"
newtype Key = Key Text
  deriving newtype (Eq, Ord, Show, Read, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

-- | Create a custom key. The input is not checked for validity.
custom :: Text -> Key
custom = Key

instance IsLabel "org.opencontainers.image.created" Key where
  fromLabel = Key "org.opencontainers.image.created"

instance IsLabel "org.opencontainers.image.authors" Key where
  fromLabel = Key "org.opencontainers.image.authors"

instance IsLabel "org.opencontainers.image.url" Key where
  fromLabel = Key "org.opencontainers.image.url"

instance IsLabel "org.opencontainers.image.documentation" Key where
  fromLabel = Key "org.opencontainers.image.documentation"

instance IsLabel "org.opencontainers.image.source" Key where
  fromLabel = Key "org.opencontainers.image.source"

instance IsLabel "org.opencontainers.image.version" Key where
  fromLabel = Key "org.opencontainers.image.version"

instance IsLabel "org.opencontainers.image.revision" Key where
  fromLabel = Key "org.opencontainers.image.revision"

instance IsLabel "org.opencontainers.image.vendor" Key where
  fromLabel = Key "org.opencontainers.image.vendor"

instance IsLabel "org.opencontainers.image.licenses" Key where
  fromLabel = Key "org.opencontainers.image.licenses"

instance IsLabel "org.opencontainers.image.ref.name" Key where
  fromLabel = Key "org.opencontainers.image.ref.name"

instance IsLabel "org.opencontainers.image.title" Key where
  fromLabel = Key "org.opencontainers.image.title"

instance IsLabel "org.opencontainers.image.description" Key where
  fromLabel = Key "org.opencontainers.image.description"

instance IsLabel "org.opencontainers.image.base.digest" Key where
  fromLabel = Key "org.opencontainers.image.base.digest"

instance IsLabel "org.opencontainers.image.base.name" Key where
  fromLabel = Key "org.opencontainers.image.base.name"
