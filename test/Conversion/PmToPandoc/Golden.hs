module Conversion.PmToPandoc.Golden (tests) where

import Conversion (Format (Pandoc, ProseMirror), convertToText)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

tests :: IO TestTree
tests = do
  return $ testGroup "ProseMirror → Pandoc (Golden)" [goldenCase "headings-and-paragraphs"]

goldenCase :: FilePath -> TestTree
goldenCase caseSubFolderPath =
  let baseDir = "test/Conversion/PmToPandoc" </> caseSubFolderPath
      pmInput = baseDir </> "pm.json"
      pandocGolden = baseDir </> "pandoc.txt"
   in goldenVsString
        caseSubFolderPath
        pandocGolden
        (toPandocNative pmInput)

toPandocNative :: FilePath -> IO BL.ByteString
toPandocNative inputFilePath = do
  pmJson <- TIO.readFile inputFilePath
  pandocText <- convertToText ProseMirror Pandoc (T.unpack pmJson)
  (return . BL.fromStrict . TE.encodeUtf8) pandocText