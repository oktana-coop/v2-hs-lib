module Diff.PmOutput.Golden (tests) where

import Conversion (Format (..))
import Diff.Utils (readFilesAndProducePmDiff)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

tests :: IO TestTree
tests = do
  return $
    testGroup
      "Markdown Inputs → ProseMirror Diff (Golden)"
      [ goldenCase "add-paragraph-end",
        goldenCase "add-paragraph-middle"
      ]

goldenCase :: FilePath -> TestTree
goldenCase caseSubFolderPath =
  let baseDir = "test/Diff/PmOutput" </> caseSubFolderPath
      md1Input = baseDir </> "doc1.md"
      md2Input = baseDir </> "doc2.md"
      pmGolden = baseDir </> "pm.json"
   in goldenVsString
        "prosemirror"
        pmGolden
        (readFilesAndProducePmDiff Markdown md1Input md2Input)
