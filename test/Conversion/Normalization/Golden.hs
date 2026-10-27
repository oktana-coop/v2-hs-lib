module Conversion.Normalization.Golden (tests) where

import Conversion.Utils (readFileAndConvert)
import Format (Format (..))
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

tests :: IO TestTree
tests = do
  return $
    testGroup
      "Normalization (Golden)"
      [ testGroup
          "meta"
          [ testGroup
              "pandoc meta types"
              [ goldenCase Markdown $ "meta" </> "pandoc-meta-types" </> "map",
                goldenCase Markdown $ "meta" </> "pandoc-meta-types" </> "list",
                goldenCase Markdown $ "meta" </> "pandoc-meta-types" </> "bool",
                -- Markdown front matter never gives a non-empty `MetaString`: Pandoc parses text values,
                -- quoted or not, into `MetaInlines` or `MetaBlocks`. So this case reads Pandoc JSON.
                goldenCase Json $ "meta" </> "pandoc-meta-types" </> "string",
                goldenCase Markdown $ "meta" </> "pandoc-meta-types" </> "inlines",
                goldenCase Markdown $ "meta" </> "pandoc-meta-types" </> "blocks"
              ],
            goldenCase Markdown $ "meta" </> "quoting",
            goldenCase Markdown $ "meta" </> "empty-values"
          ]
      ]

goldenCase :: Format -> FilePath -> TestTree
goldenCase inputFormat caseSubFolderPath =
  let baseDir = "test/Conversion/Normalization" </> caseSubFolderPath
      input = baseDir </> inputFileName
      normalizedGolden = baseDir </> "normalized.md"
   in testGroup
        caseSubFolderPath
        [ -- Normalizing reads the input with Pandoc and writes it as Markdown.
          goldenVsString
            "normalization"
            normalizedGolden
            (readFileAndConvert inputFormat Markdown id input),
          -- Normalizing the result again must change nothing.
          goldenVsString
            "idempotence"
            normalizedGolden
            (readFileAndConvert Markdown Markdown id normalizedGolden)
        ]
  where
    inputFileName :: FilePath
    inputFileName = case inputFormat of
      Json -> "doc.json"
      _ -> "doc.md"
