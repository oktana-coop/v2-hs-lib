module Diff.PmOutput.Golden (tests) where

import Conversion (Format (..))
import Diff.Utils (readFilesAndProducePmDiff)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Utils (normalizeJson)

tests :: IO TestTree
tests = do
  return $
    testGroup
      "Markdown Inputs → ProseMirror Diff (Golden)"
      [ goldenCase "add-paragraph-end",
        goldenCase "add-paragraph-middle",
        goldenCase "add-strong-emphasis-to-word",
        goldenCase "add-text-in-the-middle-of-paragraph",
        goldenCase "append-text-to-paragraph",
        goldenCase "delete-last-paragraph",
        goldenCase "delete-paragraph-middle",
        goldenCase "delete-text-from-the-end-of-paragraph",
        goldenCase "delete-text-from-the-middle-of-paragraph",
        goldenCase "remove-strong-emphasis-from-word"
      ]

goldenCase :: FilePath -> TestTree
goldenCase caseSubFolderPath =
  let baseDir = "test/Diff/PmOutput" </> caseSubFolderPath
      md1Input = baseDir </> "doc1.md"
      md2Input = baseDir </> "doc2.md"
      pmGolden = baseDir </> "pm.json"
   in goldenVsString
        caseSubFolderPath
        pmGolden
        (readFilesAndProducePmDiff Markdown normalizeJson md1Input md2Input)
