module Conversion.MdToPm.Golden (tests) where

import Conversion (Format (..))
import Conversion.Utils (readFileAndConvert)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Utils (normalizeJson)

tests :: IO TestTree
tests = do
  return $
    testGroup
      "Markdown → ProseMirror (Golden)"
      [ goldenCase "apostrophes-and-dashes",
        goldenCase "blockquote",
        goldenCase "code-blocks",
        goldenCase "headings-and-paragraphs",
        goldenCase "horizontal-rule",
        goldenCase "images-and-figures",
        goldenCase "inline-breaks",
        goldenCase "marks",
        goldenCase "lists",
        goldenCase "meta",
        goldenCase "notes",
        testGroup
          "quotes"
          [ goldenCase $ "quotes" </> "basic",
            -- Curly quote characters in the Markdown parse as `Quoted` just like straight ones.
            goldenCase $ "quotes" </> "normalization"
          ]
      ]

goldenCase :: FilePath -> TestTree
goldenCase caseSubFolderPath =
  let baseDir = "test/Conversion/MdToPm" </> caseSubFolderPath
      mdInput = baseDir </> "doc.md"
      pandocGolden = baseDir </> "pandoc.txt"
      pmGolden = baseDir </> "pm.json"
   in testGroup
        caseSubFolderPath
        [ goldenVsString
            "pandoc"
            pandocGolden
            (readFileAndConvert Markdown Pandoc id mdInput),
          goldenVsString
            "prosemirror"
            pmGolden
            (readFileAndConvert Markdown ProseMirror normalizeJson mdInput)
        ]
