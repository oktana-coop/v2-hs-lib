module ProseMirror.PandocTreeShape.UnrepresentableContent (UnrepresentableContent (..), maybeUnrepresentableBlockContent, maybeUnrepresentableInlineContent, findUnrepresentableContent, findUnrepresentableBlocks, findUnrepresentableInlines, dropUnrepresentableContent) where

import Data.Maybe (isJust, maybeToList)
import Text.Pandoc.Definition as Pandoc (Block (..), Inline (..), Pandoc)
import Text.Pandoc.Walk (query, walk)

data UnrepresentableContent
  = TableContent
  | DefinitionListContent
  | LineBlockContent
  | RawBlockContent
  | StrikeoutContent
  | UnderlineContent
  | SmallCapsContent
  | SuperscriptContent
  | SubscriptContent
  | SpanContent
  | MathContent
  | CitationContent
  | RawInlineContent
  deriving (Show, Eq, Ord)

maybeUnrepresentableBlockContent :: Pandoc.Block -> Maybe UnrepresentableContent
maybeUnrepresentableBlockContent block = case block of
  Pandoc.RawBlock _ _ -> Just RawBlockContent
  Pandoc.Table {} -> Just TableContent
  Pandoc.LineBlock _ -> Just LineBlockContent
  Pandoc.DefinitionList _ -> Just DefinitionListContent
  _ -> Nothing

maybeUnrepresentableInlineContent :: Pandoc.Inline -> Maybe UnrepresentableContent
maybeUnrepresentableInlineContent inline = case inline of
  Pandoc.Strikeout _ -> Just StrikeoutContent
  Pandoc.Underline _ -> Just UnderlineContent
  Pandoc.SmallCaps _ -> Just SmallCapsContent
  Pandoc.Superscript _ -> Just SuperscriptContent
  Pandoc.Subscript _ -> Just SubscriptContent
  Pandoc.Span _ _ -> Just SpanContent
  Pandoc.Math _ _ -> Just MathContent
  Pandoc.Cite _ _ -> Just CitationContent
  Pandoc.RawInline _ _ -> Just RawInlineContent
  _ -> Nothing

findUnrepresentableContent :: Pandoc.Pandoc -> [UnrepresentableContent]
findUnrepresentableContent doc = query (maybeToList . maybeUnrepresentableBlockContent) doc <> query (maybeToList . maybeUnrepresentableInlineContent) doc

findUnrepresentableBlocks :: Pandoc.Pandoc -> [Pandoc.Block]
findUnrepresentableBlocks = query (\block -> [block | isUnrepresentableBlock block])

findUnrepresentableInlines :: Pandoc.Pandoc -> [Pandoc.Inline]
findUnrepresentableInlines = query (\inline -> [inline | isUnrepresentableInline inline])

dropUnrepresentableContent :: Pandoc.Pandoc -> Pandoc.Pandoc
dropUnrepresentableContent = walk (filter (not . isUnrepresentableBlock)) . walk (filter (not . isUnrepresentableInline))

isUnrepresentableBlock :: Pandoc.Block -> Bool
isUnrepresentableBlock = isJust . maybeUnrepresentableBlockContent

isUnrepresentableInline :: Pandoc.Inline -> Bool
isUnrepresentableInline = isJust . maybeUnrepresentableInlineContent
