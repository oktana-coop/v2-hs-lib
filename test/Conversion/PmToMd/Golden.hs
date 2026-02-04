module Conversion.PmToMd.Golden (tests) where

import Conversion (Format (..))
import Conversion.Utils (readFileAndConvert)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

tests :: IO TestTree
tests = do
  return $
    testGroup
      "ProseMirror → Markdown (Golden)"
      [ goldenCase "blockquote",
        goldenCase "headings-and-paragraphs",
        goldenCase "marks",
        goldenCase "lists",
        goldenCase "notes"
      ]

goldenCase :: FilePath -> TestTree
goldenCase caseSubFolderPath =
  let baseDir = "test/Conversion/PmToMd" </> caseSubFolderPath
      pmInput = baseDir </> "pm.json"
      pandocGolden = baseDir </> "pandoc.txt"
      mdGolden = baseDir </> "doc.md"
   in testGroup
        caseSubFolderPath
        [ goldenVsString
            "pandoc"
            pandocGolden
            (readFileAndConvert ProseMirror Pandoc id pmInput),
          goldenVsString
            "markdown"
            mdGolden
            (readFileAndConvert ProseMirror Markdown id pmInput)
        ]