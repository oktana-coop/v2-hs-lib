module Conversion.Utils (toTextFormat, readFileAndConvert) where

import Conversion (convertToText)
import qualified Data.ByteString.Lazy as BL
import Data.List.NonEmpty (head)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Format (Format (..))
import Utils (TextNormalizer)

readFileAndConvert :: Format -> Format -> TextNormalizer -> FilePath -> IO BL.ByteString
readFileAndConvert inputFormat outputFormat normalizer inputFilePath = do
  inputText <- TIO.readFile inputFilePath
  eitherOutputText <- convertToText inputFormat outputFormat (T.unpack inputText)
  case eitherOutputText of
    Left errors -> fail ("Conversion failed: " <> (show $ Data.List.NonEmpty.head errors))
    Right outputText -> (return . BL.fromStrict . TE.encodeUtf8) (normalizer outputText)

toTextFormat :: Format -> Format -> T.Text -> IO T.Text
toTextFormat inputFormat outputFormat inputText = do
  eitherOutputText <- convertToText inputFormat outputFormat (T.unpack inputText)
  case eitherOutputText of
    Left errors -> fail ("Conversion failed: " <> (show $ Data.List.NonEmpty.head errors))
    Right outputText -> pure outputText
