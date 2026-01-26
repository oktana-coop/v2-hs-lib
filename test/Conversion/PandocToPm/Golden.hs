module Conversion.PandocToPm.Golden (tests) where

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
  return $ testGroup "Pandoc → ProseMirror (Golden)" [goldenCase "headings-and-paragraphs", goldenCase "marks"]

goldenCase :: FilePath -> TestTree
goldenCase caseSubFolderPath =
  let baseDir = "test/Conversion/PandocToPm" </> caseSubFolderPath
      pmInput = baseDir </> "pandoc.txt"
      pandocGolden = baseDir </> "pm.json"
   in goldenVsString
        caseSubFolderPath
        pandocGolden
        (toPmJson pmInput)

toPmJson :: FilePath -> IO BL.ByteString
toPmJson inputFilePath = do
  pancodNative <- TIO.readFile inputFilePath
  pmJson <- convertToText Pandoc ProseMirror (T.unpack pancodNative)
  (return . BL.fromStrict . TE.encodeUtf8) pmJson