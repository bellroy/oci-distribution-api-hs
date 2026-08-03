-- | Helpers related to retrieving layers by a specific annotation, such as
-- @#"org.opencontainers.image.title"@ (which ORAS uses for file names)
module OpenContainerImage.Flow.GetLayerByAnnotation
  ( lookupLayerDigestByAnnotation,
    layerDigestsByAnnotationKeyMap,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, maybeToList)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import OpenContainerImage.Manifest
import OpenContainerImage.Manifest.Annotation qualified as Annotation

-- | Create a map of annotation values to their corresponding digest.
--
-- Example: create a map of digests for layers with
-- #"org.opencontainers.image.title" as @"test.txt"@
--
-- @
--   OCI.layerDigestByAnnotation #"org.opencontainers.image.title" (== "test.txt") layers
-- @
layerDigestsByAnnotationKeyMap :: Annotation.Key -> (Text -> Bool) -> Vector Descriptor -> Map Text Digest
layerDigestsByAnnotationKeyMap key checkValue layers =
  Map.fromList (layerDigestsByAnnotationList key checkValue layers)

-- | Get a digest with a given key and value for that key.
--
-- Prefer 'layerDigestsByAnnotationKeyMap' if ever needing to make multiple
-- calls to this for a single manifest.
lookupLayerDigestByAnnotation :: Annotation.Key -> (Text -> Bool) -> Vector Descriptor -> Maybe (Text, Digest)
lookupLayerDigestByAnnotation key checkValue layers =
  listToMaybe (layerDigestsByAnnotationList key checkValue layers)

layerDigestsByAnnotationList :: Annotation.Key -> (Text -> Bool) -> Vector Descriptor -> [(Text, Digest)]
layerDigestsByAnnotationList key checkValue layers =
  [ (layerKeyValue, digest)
  | Descriptor {digest, annotations = Just annotations} <- Vector.toList layers,
    layerKeyValue <- maybeToList (Map.lookup key annotations),
    checkValue layerKeyValue
  ]
