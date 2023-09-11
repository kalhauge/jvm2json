{-# LANGUAGE BlockArguments #-}
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
getToJsonTests = do
  files <- getDirectoryContents "test/examples/classes"
  pure $
    testGroup
      "ToJson"
      [ goldenVsFile
        fp
        ("test/examples/expected" </> fp -<.> "json")
        out
        do
          runJvm2Json Config{classFile = PathToFile inp, jsonFile = PathToFile out} OutputJSON
      | fp <- files
      , takeExtension fp == ".class"
      , let
          inp = "test/examples/classes" </> fp
          out = "test/examples/expected" </> fp -<.> "json.tmp"
      ]
