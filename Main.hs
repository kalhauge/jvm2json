{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- base
import Data.String
import System.IO (hPrint, stderr)

-- aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Aeson.Internal as Aeson
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Aeson.Parser.Internal as Aeson

-- bytestring
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Foldable

import Jvmhs (deserializeClass, serializeClass)
import Jvmhs.Format.Codec

import Options.Applicative

data Action
  = OutputJSON
  | InputJSON

data Target
  = PathToFile FilePath
  | StdIO

instance IsString Target where
  fromString = \case
    "-" -> StdIO
    f -> PathToFile f

readTarget :: Target -> IO BL.ByteString
readTarget = \case
  PathToFile f -> BL.readFile f
  StdIO -> BL.getContents

writeTarget :: Target -> BL.ByteString -> IO ()
writeTarget = \case
  PathToFile f -> BL.writeFile f
  StdIO -> BL.putStr

data Config = Config
  { classFile :: Target
  , jsonFile :: Target
  }

parseConfig :: Parser (Config, Action)
parseConfig = do
  classFile <-
    option str . fold $
      [ short 's'
      , long "classfile"
      , help "the path to the classfile, - for stdin/out"
      , metavar "CLASSFILE"
      , value StdIO
      ]
  jsonFile <-
    option str . fold $
      [ short 't'
      , long "json"
      , help "the path to the jsonfile, - for stdin/out"
      , metavar "JSONFILE"
      , value StdIO
      ]
  action <-
    flag OutputJSON InputJSON . fold $
      [ short 'R'
      , long "reverse"
      , help "input json and output a jvm class"
      ]
  pure (Config{..}, OutputJSON)

main :: IO ()
main = do
  (config, act) <-
    execParser . info (parseConfig <**> helper) . fold $
      [ progDesc "a tool for converting between java classfile and json"
      ]
  case act of
    OutputJSON -> do
      bytes <- readTarget (classFile config)
      case deserializeClass True bytes of
        Right cls -> do
          v <- either fail pure $ toEncodingClass cls
          writeTarget (jsonFile config) (Aeson.encodingToLazyByteString v)
        Left err ->
          hPrint stderr err
    InputJSON -> do
      bytes <- readTarget (jsonFile config)
      case Aeson.eitherDecodeWith Aeson.jsonEOF' (Aeson.iparse parseJSONClass) bytes of
        Right cls -> do
          writeTarget (classFile config) (serializeClass cls)
        Left err ->
          print err
