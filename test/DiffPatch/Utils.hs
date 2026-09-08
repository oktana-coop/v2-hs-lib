module DiffPatch.Utils (readFilesAndProducePmDiff, readFilesAndProducePmSteps) where

import Data.Aeson (ToJSON)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as BL
import Data.List.NonEmpty (NonEmpty, head)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as LT
import Diffing (proseMirrorDiff)
import Patching (proseMirrorSteps)
import Format (Format)
import Text.Pandoc (PandocError)
import Utils (TextNormalizer)

toJsonText :: (ToJSON a) => a -> T.Text
toJsonText = LT.toStrict . encodeToLazyText

readFilesAndProducePmDiff :: Format -> TextNormalizer -> FilePath -> FilePath -> IO BL.ByteString
readFilesAndProducePmDiff inputFormat = readFilesAndProduce (proseMirrorDiff inputFormat)

readFilesAndProducePmSteps :: Format -> Format -> TextNormalizer -> FilePath -> FilePath -> IO BL.ByteString
readFilesAndProducePmSteps beforeFormat afterFormat = readFilesAndProduce (proseMirrorSteps beforeFormat afterFormat)

readFilesAndProduce :: (ToJSON a) => (String -> String -> IO (Either (NonEmpty PandocError) a)) -> TextNormalizer -> FilePath -> FilePath -> IO BL.ByteString
readFilesAndProduce produce normalizer input1FilePath input2FilePath = do
  input1Text <- TIO.readFile input1FilePath
  input2Text <- TIO.readFile input2FilePath
  eitherResult <- produce (T.unpack input1Text) (T.unpack input2Text)
  case eitherResult of
    Left errors -> fail ("Conversion failed: " <> (show $ Data.List.NonEmpty.head errors))
    -- We first convert the result to JSON text to use the normalizer (which takes T.Text inputs)
    -- TODO: Improve this, potentially by making normalizer take an input of `ToJSON` type.
    Right result -> (return . BL.fromStrict . TE.encodeUtf8 . normalizer . toJsonText) result
