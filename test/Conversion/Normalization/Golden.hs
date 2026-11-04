{-# LANGUAGE OverloadedStrings #-}

module Conversion.Normalization.Golden (tests) where

import Conversion.Utils (readFileAndConvert, toTextFormat)
import Data.Aeson (Value (..), decodeStrictText)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Format (Format (..))
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.Golden.Advanced (goldenTest)

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
            (readFileAndConvert Markdown Markdown id normalizedGolden),
          -- The ProseMirror doc's attribute holds the front matter of the normalized document. The golden is the
          -- normalization's, so accepting leaves it as it is.
          goldenTest
            "prosemirror front matter"
            (frontMatterOf <$> TIO.readFile normalizedGolden)
            (proseMirrorFrontMatter inputFormat input)
            compareFrontMatter
            (const (pure ()))
        ]
  where
    inputFileName :: FilePath
    inputFileName = case inputFormat of
      Json -> "doc.json"
      _ -> "doc.md"

-- The YAML between the delimiters of a Markdown document's front matter.
frontMatterOf :: T.Text -> Maybe T.Text
frontMatterOf markdown = case T.lines markdown of
  "---" : rest -> Just (T.unlines (takeWhile (/= "---") rest))
  _ -> Nothing

-- The front matter in the doc's attribute of the input converted to ProseMirror.
proseMirrorFrontMatter :: Format -> FilePath -> IO (Maybe T.Text)
proseMirrorFrontMatter inputFormat inputFilePath = do
  input <- TIO.readFile inputFilePath
  pmJson <- toTextFormat inputFormat ProseMirror input
  pure (docFrontMatter pmJson)
  where
    docFrontMatter :: T.Text -> Maybe T.Text
    docFrontMatter pmJson = do
      Object doc <- decodeStrictText pmJson
      Object attrs <- KeyMap.lookup "attrs" doc
      String frontMatter <- KeyMap.lookup "frontMatter" attrs
      pure frontMatter

compareFrontMatter :: Maybe T.Text -> Maybe T.Text -> IO (Maybe String)
compareFrontMatter expected actual
  | expected == actual = pure Nothing
  | otherwise = pure (Just ("expected " <> show expected <> ", got " <> show actual))
