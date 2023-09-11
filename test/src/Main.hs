{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.Golden as G

import qualified Data.Text.Lazy.Encoding as Text

import System.Process.Typed

import Prettyprinter
import Prettyprinter.Render.Text

import Codec
import Jvm2Json
import System.Directory
import System.FilePath

main :: IO ()
main = do
  toJson <- getToJsonTests
  defaultMain $
    testGroup
      "Tests"
      [ codec
      , usage
      , toJson
      ]

codec :: TestTree
codec =
  goldenVsString
    "codec"
    "CODEC.txt"
    (pure . Text.encodeUtf8 . renderLazy $ layoutPretty defaultLayoutOptions (prettyClass <> line))

usage :: TestTree
usage =
  goldenVsString
    "codec"
    "USAGE.txt"
    ( readProcessStdout_ "cabal run jvm2json -- --help"
    )

getToJsonTests :: IO TestTree
getToJsonTests =
  findExecutable "jq" >>= \case
    Just jq -> do
      files <- findByExtension [".class"] "test/examples/classes"
      pure $
        testGroup
          "ToJson"
          [ goldenVsString
            fp
            ("test/examples/expected" </> makeRelative "test/examples/classes" fp -<.> "json")
            do
              result <- readProcessStdout_ (proc "cabal" ["run", "jvm2json", "--", "-s", fp])
              readProcessStdout_ (setStdin (byteStringInput result) (proc jq ["."]))
          | fp <- files
          ]
    Nothing ->
      pure $ testGroup "ToJson" []
