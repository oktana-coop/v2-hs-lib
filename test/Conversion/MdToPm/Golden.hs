module Conversion.MdToPm.Golden (tests) where

import Conversion (Format (..))
import Conversion.Utils (toTextFormat)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

tests :: IO TestTree
tests = do
  return $ testGroup "Markdown → ProseMirror (Golden)" [goldenCase "headings-and-paragraphs", goldenCase "marks"]

goldenCase :: FilePath -> TestTree
goldenCase caseSubFolderPath =
  let baseDir = "test/Conversion/MdToPm" </> caseSubFolderPath
      mdInput = baseDir </> "doc.md"
      pandocGolden = baseDir </> "pandoc.txt"
      pmGolden = baseDir </> "pm.json"
   in testGroup
        caseSubFolderPath
        [ goldenVsString
            caseSubFolderPath
            pandocGolden
            (toTextFormat Markdown Pandoc mdInput),
          goldenVsString
            caseSubFolderPath
            pmGolden
            (toTextFormat Markdown ProseMirror mdInput)
        ]
