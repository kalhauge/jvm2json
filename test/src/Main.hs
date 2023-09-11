{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.Golden as G

import qualified Data.Text.Lazy.Encoding as Text

import System.Process.Typed

import Prettyprinter
import Prettyprinter.Render.Text

import Codec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ codec
    , usage
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
