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
      [ goldenCase "apostrophes-and-dashes",
        goldenCase "blockquote",
        goldenCase "code-blocks",
        goldenCase "headings-and-paragraphs",
        goldenCase "horizontal-rule",
        goldenCase "images-and-figures",
        goldenCase "marks",
        goldenCase "lists",
        goldenCase "meta",
        goldenCase "notes",
        testGroup
          "quotes"
          [ goldenCase $ "quotes" </> "basic",
            -- Curly quote characters in the PM text reach the writer as plain `Str`
            -- text (no `Quoted` node is reconstructed). Then, the writer normalizes them
            -- to straight quotes because Pandoc's Ext_smart extension is on in the writer options.
            goldenCase $ "quotes" </> "normalization"
          ]
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