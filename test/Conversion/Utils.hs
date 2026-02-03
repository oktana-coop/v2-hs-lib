module Conversion.Utils (toTextFormat, readFileAndConvert, normalizeJson) where

import Conversion (Format, convertToText)
import Data.Aeson (Value, decode)
import Data.Aeson.Encode.Pretty (Config, confCompare, defConfig, encodePretty')
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

readFileAndConvert :: Format -> Format -> FilePath -> IO BL.ByteString
readFileAndConvert inputFormat outputFormat inputFilePath = do
  inputText <- TIO.readFile inputFilePath
  eitherOutputText <- convertToText inputFormat outputFormat (T.unpack inputText)
  case eitherOutputText of
    Left err -> fail ("Conversion failed: " <> show err)
    Right outputText -> (return . BL.fromStrict . TE.encodeUtf8) outputText

toTextFormat :: Format -> Format -> T.Text -> IO T.Text
toTextFormat inputFormat outputFormat inputText = do
  eitherOutputText <- convertToText inputFormat outputFormat (T.unpack inputText)
  case eitherOutputText of
    Left err -> fail ("Conversion failed: " <> show err)
    Right outputText -> pure outputText

normalizeJson :: T.Text -> T.Text
normalizeJson input = case decode (BL.fromStrict $ TE.encodeUtf8 input) :: Maybe Value of
  -- Return original text if invalid JSON
  Nothing -> input
  Just value -> TE.decodeUtf8 $ BL.toStrict (encodePretty' prettyConfig value)
  where
    prettyConfig :: Data.Aeson.Encode.Pretty.Config
    prettyConfig = defConfig {confCompare = compare}