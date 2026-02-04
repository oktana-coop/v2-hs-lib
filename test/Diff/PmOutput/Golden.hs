module Diff.PmOutput.Golden (tests) where

import Conversion (Format (..))
import Diff.Utils (readFilesAndProducePmDiff)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Utils (normalizeJson)

tests :: IO TestTree
tests = do
  return
    $ testGroup
      "Markdown Inputs → ProseMirror Diff (Golden)"
    $ [ testGroup
          "adding-and-removing-paragraphs"
          [ goldenCase $ "adding-and-removing-paragraphs" </> "add-paragraph-end",
            goldenCase $ "adding-and-removing-paragraphs" </> "add-paragraph-middle",
            goldenCase $ "adding-and-removing-paragraphs" </> "delete-last-paragraph",
            goldenCase $ "adding-and-removing-paragraphs" </> "delete-paragraph-middle"
          ],
        testGroup
          "inline-text-updates"
          [ goldenCase $ "inline-text-updates" </> "add-text-in-the-middle-of-paragraph",
            goldenCase $ "inline-text-updates" </> "append-text-to-paragraph",
            goldenCase $ "inline-text-updates" </> "delete-text-from-the-end-of-paragraph",
            goldenCase $ "inline-text-updates" </> "delete-text-from-the-middle-of-paragraph"
          ],
        testGroup
          "marks"
          [ goldenCase $ "marks" </> "add-strong-emphasis-to-word",
            goldenCase $ "marks" </> "remove-strong-emphasis-from-word"
          ]
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
