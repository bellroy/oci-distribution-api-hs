{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Map.Strict qualified as Map
import Network.HTTP.Client.TLS qualified as HTTP (newTlsManager)
import OpenContainerImage.Flow.GetLayerByAnnotation qualified as OCI
import OpenContainerImage.Manifest qualified as OCI
import OpenContainerImage.Registry qualified as OCI
import Optics
import Options.Applicative qualified as O
import Options.Applicative.Help.Pretty qualified as O
import Options.Generic qualified as O
import Relude
import Prelude ()

data Args = Args
  { baseUrl,
    manifest,
    ref,
    layer ::
      Text
  }
  deriving stock (Generic)
  deriving anyclass (O.ParseRecord)

docstr :: O.Doc
docstr =
  """
  Example:
    1. Start registry: `podman run -d -p 5000:5000 registry:2`
    2. Push OCI artifact with an annotated layer
        ```
        echo "hello" > hello.txt
        oras push localhost:5000/test/artifact:v1 --plain-http hello.txt:text/plain
        ```
    3. Fetch that layer back (prints "hello"):
      oci-read-one-layer -- --baseUrl http://localhost:5000 --manifest test/artifact --ref v1 --layer hello.txt
  """

main :: IO ()
main = do
  Args {baseUrl, manifest = manifestName, ref, layer = layerName} <- O.getRecordWith (O.progDescDoc (Just docstr)) mempty
  manager <- HTTP.newTlsManager
  client <- OCI.newClient baseUrl manager >>= maybe (fail "nope") pure
  putTextLn "retrieving manifest..."
  manifest <- OCI.getImageManifest client manifestName ref >>= either (fail . show) pure
  let layers =
        OCI.layerDigestsByAnnotationKeyMap
          #"org.opencontainers.image.title"
          (const True)
          (case manifest of OCI.ImageManifest {layers} -> layers)

  putTextLn (fold ["available layers:", show (Map.keys layers)])

  digest <-
    layers
      ^. at layerName
      & maybe (fail "digest not found") pure

  putTextLn $ fold ["retrieving blob ", show layerName, "..."]
  OCI.withBlobFromDigest
    client
    manifestName
    digest
    ( \response -> do
        content <- OCI.consumeBody response
        putTextLn "# blob contents"
        putLBSLn content
    )
    >>= either (fail . show) pure
  pass
