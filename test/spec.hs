{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson qualified as JSON (decodeFileStrict)
import Data.Aeson.Encode.Pretty qualified as JSON (encodePretty)
import Data.ByteString.Lazy qualified as LBS
import OpenContainerImage.Manifest (ImageManifest)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsFile)

main :: IO ()
main =
  defaultMain . testGroup "tests" $
    [ goldenVsFile
        "FromJSON ~ ToJSON"
        "test/example.in.json"
        "test/example.out.json"
        ( do
            Just p <- JSON.decodeFileStrict @ImageManifest "test/example.in.json"
            LBS.writeFile "test/example.out.json" (JSON.encodePretty p)
        )
    ]
