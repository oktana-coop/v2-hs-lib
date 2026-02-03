module Conversion.MdToPm.Golden (tests) where

import Conversion (Format (..))
import Conversion.Utils (readFileAndConvert)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

tests :: IO TestTree
tests = do
  return $ testGroup "Markdown → ProseMirror (Golden)" [goldenCase "blockquote", goldenCase "headings-and-paragraphs", goldenCase "marks", goldenCase "lists"]

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
            (readFileAndConvert Markdown Pandoc mdInput),
          goldenVsString
            "prosemirror"
            pmGolden
            (readFileAndConvert Markdown ProseMirror mdInput)
        ]
