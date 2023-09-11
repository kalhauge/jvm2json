import Test.Tasty
import Test.Tasty.Golden as G

import qualified Data.Text.Lazy.Encoding as Text

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
    ]

codec :: TestTree
codec =
  goldenVsString
    "codec"
    "CODEC.txt"
    (pure . Text.encodeUtf8 . renderLazy $ layoutPretty defaultLayoutOptions prettyClass)
