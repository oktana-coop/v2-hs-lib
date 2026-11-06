{-# LANGUAGE OverloadedStrings #-}

module Markdown.FrontMatter.Text (frontMatterText, isClosingDelimiter, splitFrontMatter, wrapInFrontMatterDelimiters) where

import Data.Char (isSpace)
import qualified Data.Text as T

frontMatterText :: T.Text -> Maybe T.Text
frontMatterText = fmap fst . splitFrontMatter

-- The text between the delimiters of a document's front matter, and the document after it, following Pandoc's
-- rules for metadata blocks. The document after it can still have metadata, in YAML blocks further down.
splitFrontMatter :: T.Text -> Maybe (T.Text, T.Text)
splitFrontMatter input = case withoutLeadingBlankLines (documentLines input) of
  opening : afterOpening@(next : _) | opensMetadataBlock opening next -> splitAtClosingDelimiter afterOpening
  _ -> Nothing
  where
    -- Pandoc reads `---` followed by a blank line as a horizontal rule, not as the start of a metadata block.
    opensMetadataBlock :: T.Text -> T.Text -> Bool
    opensMetadataBlock opening next = isOpeningDelimiter opening && not (isBlank next)

    documentLines :: T.Text -> [T.Text]
    documentLines = map withoutCarriageReturn . T.lines . withoutByteOrderMark

    -- Some editors, mostly on Windows, start a file with one.
    withoutByteOrderMark :: T.Text -> T.Text
    withoutByteOrderMark = T.dropWhile (== '\xFEFF')

    -- What a Windows line ending leaves at the end of a line.
    withoutCarriageReturn :: T.Text -> T.Text
    withoutCarriageReturn = T.dropWhileEnd (== '\r')

    -- Pandoc ignores blank lines at the start of a document, so front matter can come after some.
    withoutLeadingBlankLines :: [T.Text] -> [T.Text]
    withoutLeadingBlankLines = dropWhile isBlank

    splitAtClosingDelimiter :: [T.Text] -> Maybe (T.Text, T.Text)
    splitAtClosingDelimiter ls = case break isClosingDelimiter ls of
      (yamlLines, _ : linesAfter) -> Just (T.unlines yamlLines, T.unlines linesAfter)
      _ -> Nothing

    isOpeningDelimiter :: T.Text -> Bool
    isOpeningDelimiter line = T.stripEnd line == "---"

    isBlank :: T.Text -> Bool
    isBlank = T.all isSpace

-- A line that ends a front matter block, following Pandoc's rules for metadata blocks.
isClosingDelimiter :: T.Text -> Bool
isClosingDelimiter line = T.stripEnd line `elem` ["---", "..."]

-- The YAML between delimiter lines, as a Markdown document of the front matter alone.
wrapInFrontMatterDelimiters :: T.Text -> T.Text
wrapInFrontMatterDelimiters yaml = T.unlines ("---" : T.lines yaml <> ["---"])
