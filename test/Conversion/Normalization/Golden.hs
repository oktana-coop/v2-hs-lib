module Conversion.Normalization.Golden (NormalizationCase (..), inputPath, metaNormalizationCases, normalizedPath, tests) where

import Conversion.Utils (readFileAndConvert)
import Format (Format (..))
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

-- A folder with an input document and the golden of its normalization.
data NormalizationCase = NormalizationCase {inputFormat :: Format, caseSubFolderPath :: FilePath}

metaNormalizationCases :: [NormalizationCase]
metaNormalizationCases =
  [ NormalizationCase Markdown $ "meta" </> "pandoc-meta-types" </> "map",
    NormalizationCase Markdown $ "meta" </> "pandoc-meta-types" </> "list",
    NormalizationCase Markdown $ "meta" </> "pandoc-meta-types" </> "bool",
    -- Markdown front matter never gives a non-empty `MetaString`: Pandoc parses text values,
    -- quoted or not, into `MetaInlines` or `MetaBlocks`. So this case reads Pandoc JSON.
    NormalizationCase Json $ "meta" </> "pandoc-meta-types" </> "string",
    NormalizationCase Markdown $ "meta" </> "pandoc-meta-types" </> "inlines",
    NormalizationCase Markdown $ "meta" </> "pandoc-meta-types" </> "blocks",
    NormalizationCase Markdown $ "meta" </> "quoting",
    NormalizationCase Markdown $ "meta" </> "empty-values"
  ]

inputPath :: NormalizationCase -> FilePath
inputPath normalizationCase = caseFolder normalizationCase </> inputFileName
  where
    inputFileName :: FilePath
    inputFileName = case inputFormat normalizationCase of
      Json -> "doc.json"
      _ -> "doc.md"

normalizedPath :: NormalizationCase -> FilePath
normalizedPath normalizationCase = caseFolder normalizationCase </> "normalized.md"

caseFolder :: NormalizationCase -> FilePath
caseFolder normalizationCase = "test/Conversion/Normalization" </> caseSubFolderPath normalizationCase

tests :: IO TestTree
tests = do
  return $
    testGroup
      "Normalization (Golden)"
      [testGroup "meta" (map goldenCase metaNormalizationCases)]

goldenCase :: NormalizationCase -> TestTree
goldenCase normalizationCase =
  let input = inputPath normalizationCase
      normalizedGolden = normalizedPath normalizationCase
   in testGroup
        (caseSubFolderPath normalizationCase)
        [ -- Normalizing reads the input with Pandoc and writes it as Markdown.
          goldenVsString
            "normalization"
            normalizedGolden
            (readFileAndConvert (inputFormat normalizationCase) Markdown id input),
          -- Normalizing the result again must change nothing.
          goldenVsString
            "idempotence"
            normalizedGolden
            (readFileAndConvert Markdown Markdown id normalizedGolden)
        ]
