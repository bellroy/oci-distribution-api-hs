-- | Helper related to retrieving layers by a specific annotation, such as
-- @#"org.opencontainers.image.title"@ (which ORAS uses for file names)
module OpenContainerImage.Flow.GetLayerByAnnotation
  ( makeLayersByAnnotationMap,
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

-- | Create a map of annotation values to their corresponding descriptor.
--
-- Example: create a map of descriptors for layers with
-- #"org.opencontainers.image.title" and lookup files @"test.txt"@ @"test2.txt"@
--
-- @
--   let
--     layers = OCI.makeLayersByAnnotationMap #"org.opencontainers.image.title" layers
--     layer1 = layers ^. at "test.txt"
--     layer2 = layers ^. at "test2.txt"
--   in
--     _
-- @
makeLayersByAnnotationMap :: Annotation.Key -> Vector Descriptor -> Map Text Descriptor
makeLayersByAnnotationMap key layers =
  Map.fromList
    [ (layerKeyValue, descriptor)
    | descriptor@Descriptor {annotations = Just annotations} <- Vector.toList layers,
      layerKeyValue <- maybeToList (Map.lookup key annotations)
    ]
