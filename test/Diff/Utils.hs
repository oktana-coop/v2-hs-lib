module Diff.Utils (readFilesAndProducePmDiff) where

import Data.Aeson (ToJSON)
import qualified Data.ByteString.Lazy as BL
import Data.List.NonEmpty (head)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Diff (proseMirrorDiff)
import Format (Format)
import Utils (TextNormalizer)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as TE

toJsonText :: ToJSON a => a -> T.Text
toJsonText = LT.toStrict . encodeToLazyText

readFilesAndProducePmDiff :: Format -> TextNormalizer -> FilePath -> FilePath -> IO BL.ByteString
readFilesAndProducePmDiff inputFormat normalizer input1FilePath input2FilePath = do
  input1Text <- TIO.readFile input1FilePath
  input2Text <- TIO.readFile input2FilePath
  eitherDecoratedPMDoc <- proseMirrorDiff inputFormat (T.unpack input1Text) (T.unpack input2Text)
  case eitherDecoratedPMDoc of
    Left errors -> fail ("Conversion failed: " <> (show $ Data.List.NonEmpty.head errors))
    -- We first convert the decorated PM Doc to JSON text to use the normalizer (which takes T.Text inputs)
    -- TODO: Improve this, potentially by making normalizer take an input of `ToJSON` type.
    Right decoratedPMDoc -> (return . BL.fromStrict . TE.encodeUtf8 . normalizer . toJsonText) decoratedPMDoc