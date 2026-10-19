{-# LANGUAGE OverloadedStrings #-}

module Markdown.FrontMatter (frontMatterKeyOutline, orderFrontMatter) where

import Data.Char (isSpace)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import Metadata.KeyOutline (KeyOutline (..), listItemsWithOutlines, mapEntriesInOutlineOrder)
import Yamlet.Syntax (Content (..), Document (..), Node (..), defaultRenderOptions, parseDocumentsText, renderSyntax)

-- The key outline of the YAML block a Markdown document starts with.
frontMatterKeyOutline :: T.Text -> KeyOutline
frontMatterKeyOutline input = case frontMatterText input >>= parseExactlyOneYamlDocument of
  Just Document {root = rootNode} -> nodeOutline rootNode
  Nothing -> NoOutline
  where
    -- The syntax tree has the entries of a mapping and the items of a sequence in the order they are written,
    -- which is the order the outline keeps.
    nodeOutline :: Node -> KeyOutline
    nodeOutline Node {content = MappingContent _ entries} = MapOutline (mapMaybe entryOutline entries)
    nodeOutline Node {content = SequenceContent _ items} = ListOutline (map nodeOutline items)
    nodeOutline _ = NoOutline

    -- The key of an entry as written, which is how Pandoc names it, also when YAML reads it as another value
    -- than a string, e.g. `2024`.
    entryOutline :: (Node, Node) -> Maybe (T.Text, KeyOutline)
    entryOutline (Node {content = ScalarLinesContent _ keyText _}, value) = Just (keyText, nodeOutline value)
    -- A key that is not a scalar, e.g. `? [a, b]`, is left out of the outline.
    entryOutline _ = Nothing

-- The text between the delimiters of a document's front matter, following Pandoc's rules for metadata blocks.
frontMatterText :: T.Text -> Maybe T.Text
frontMatterText input = case withoutLeadingBlankLines (documentLines input) of
  opening : afterOpening@(next : _) | opensMetadataBlock opening next -> textUntilClosingDelimiter afterOpening
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

    textUntilClosingDelimiter :: [T.Text] -> Maybe T.Text
    textUntilClosingDelimiter ls = case break isClosingDelimiter ls of
      (yamlLines, _ : _) -> Just (T.unlines yamlLines)
      _ -> Nothing

    isOpeningDelimiter :: T.Text -> Bool
    isOpeningDelimiter line = T.stripEnd line == "---"

    isClosingDelimiter :: T.Text -> Bool
    isClosingDelimiter line = T.stripEnd line `elem` ["---", "..."]

    isBlank :: T.Text -> Bool
    isBlank = T.all isSpace

-- Puts the front matter of Pandoc's Markdown output, which Pandoc writes with the keys sorted, in the order of the metadata's key outline.
-- Only whole entries move, and yamlet writes the YAML the way Pandoc does.
orderFrontMatter :: KeyOutline -> T.Text -> T.Text
orderFrontMatter NoOutline markdown = markdown
orderFrontMatter outline markdown = fromMaybe markdown (reorderPandocFrontMatter markdown)
  where
    reorderPandocFrontMatter :: T.Text -> Maybe T.Text
    reorderPandocFrontMatter pandocOutput = do
      (pandocFrontMatter, body) <- splitPandocFrontMatter pandocOutput
      document <- parseExactlyOneYamlDocument pandocFrontMatter
      pure (delimiterLine <> render (orderYamlByOutline document) <> delimiterLine <> body)

    -- Pandoc writes the front matter at the start of its output: `---`, the YAML, and `---`, each on its own lines.
    splitPandocFrontMatter :: T.Text -> Maybe (T.Text, T.Text)
    splitPandocFrontMatter pandocOutput = do
      afterOpening <- T.stripPrefix delimiterLine pandocOutput
      let (yamlBeforeLastNewline, closing) = T.breakOn ("\n" <> delimiterLine) afterOpening
      body <- T.stripPrefix ("\n" <> delimiterLine) closing
      Just (yamlBeforeLastNewline <> "\n", body)

    delimiterLine :: T.Text
    delimiterLine = "---\n"

    orderYamlByOutline :: Document -> Document
    orderYamlByOutline document@Document {root = rootNode} = document {root = orderYamlNodeByOutline outline rootNode}

    render :: Document -> T.Text
    render document = renderSyntax defaultRenderOptions [document]

orderYamlNodeByOutline :: KeyOutline -> Node -> Node
orderYamlNodeByOutline outline node@Node {content = nodeContent} = node {content = orderContent nodeContent}
  where
    orderContent :: Content -> Content
    orderContent (MappingContent style entries) = MappingContent style (orderMappingEntries entries)
    orderContent (SequenceContent style items) = SequenceContent style (map orderItem (listItemsWithOutlines outline items))
    orderContent other = other

    -- Only the entries of a mapping whose keys are all scalars, none of them repeated, can be ordered by key.
    orderMappingEntries :: [(Node, Node)] -> [(Node, Node)]
    orderMappingEntries entries
      | M.size entriesByKey == length entries = map orderEntry (mapEntriesInOutlineOrder outline entriesByKey)
      | otherwise = entries
      where
        entriesByKey = M.fromList (mapMaybe keyedEntry entries)

    orderEntry :: (T.Text, (Node, Node), KeyOutline) -> (Node, Node)
    orderEntry (_, (key, value), valueOutline) = (key, orderYamlNodeByOutline valueOutline value)

    orderItem :: (Node, KeyOutline) -> Node
    orderItem (item, itemOutline) = orderYamlNodeByOutline itemOutline item

    -- An entry with the text of its key, when the key is a scalar.
    keyedEntry :: (Node, Node) -> Maybe (T.Text, (Node, Node))
    keyedEntry entry@(Node {content = ScalarLinesContent _ keyText _}, _) = Just (keyText, entry)
    keyedEntry _ = Nothing

-- Nothing for text that is not valid YAML or holds more than one document.
parseExactlyOneYamlDocument :: T.Text -> Maybe Document
parseExactlyOneYamlDocument yaml = case parseDocumentsText yaml of
  Right [document] -> Just document
  _ -> Nothing
