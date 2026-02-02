module Conversion.Utils (toTextFormat) where

import Conversion (Format, convertToText)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

toTextFormat :: Format -> Format -> FilePath -> IO BL.ByteString
toTextFormat inputFormat outputFormat inputFilePath = do
  pmJson <- TIO.readFile inputFilePath
  eitherMdText <- convertToText inputFormat outputFormat (T.unpack pmJson)
  case eitherMdText of
    Left err -> fail ("Conversion failed: " <> show err)
    Right mdText -> (return . BL.fromStrict . TE.encodeUtf8) mdText