module Conversion.PmToMd.Golden (tests) where

import Conversion (Format (..))
import Conversion.Utils (toTextFormat)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

tests :: IO TestTree
tests = do
  return $ testGroup "ProseMirror → Markdown (Golden)" [goldenCase "headings-and-paragraphs", goldenCase "marks"]

goldenCase :: FilePath -> TestTree
goldenCase caseSubFolderPath =
  let baseDir = "test/Conversion/PmToMd" </> caseSubFolderPath
      pmInput = baseDir </> "pm.json"
      pandocGolden = baseDir </> "pandoc.txt"
      mdGolden = baseDir </> "doc.md"
   in testGroup
        caseSubFolderPath
        [ goldenVsString
            caseSubFolderPath
            pandocGolden
            (toTextFormat ProseMirror Pandoc pmInput),
          goldenVsString
            caseSubFolderPath
            mdGolden
            (toTextFormat ProseMirror Markdown pmInput)
        ]