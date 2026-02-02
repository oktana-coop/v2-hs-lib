module Conversion.Utils (toTextFormat, readFileAndConvert) where

import Conversion (Format, convertToText)
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