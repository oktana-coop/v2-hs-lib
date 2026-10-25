module ProseMirror.PandocTreeShape.UnrepresentableBlocks (UnrepresentableContent (..), maybeUnrepresentableContent, findUnrepresentableContent, findUnrepresentableBlocks, dropUnrepresentableBlocks) where

import Data.Maybe (isJust, maybeToList)
import Text.Pandoc.Definition as Pandoc (Block (..), Pandoc)
import Text.Pandoc.Walk (query, walk)

data UnrepresentableContent
  = TableContent
  | DefinitionListContent
  | LineBlockContent
  | RawBlockContent
  deriving (Show, Eq, Ord)

maybeUnrepresentableContent :: Pandoc.Block -> Maybe UnrepresentableContent
maybeUnrepresentableContent block = case block of
  Pandoc.RawBlock _ _ -> Just RawBlockContent
  Pandoc.Table {} -> Just TableContent
  Pandoc.LineBlock _ -> Just LineBlockContent
  Pandoc.DefinitionList _ -> Just DefinitionListContent
  _ -> Nothing

findUnrepresentableContent :: Pandoc.Pandoc -> [UnrepresentableContent]
findUnrepresentableContent = query (maybeToList . maybeUnrepresentableContent)

findUnrepresentableBlocks :: Pandoc.Pandoc -> [Pandoc.Block]
findUnrepresentableBlocks = query (\block -> [block | isUnrepresentable block])

dropUnrepresentableBlocks :: Pandoc.Pandoc -> Pandoc.Pandoc
dropUnrepresentableBlocks = walk (filter (not . isUnrepresentable))

isUnrepresentable :: Pandoc.Block -> Bool
isUnrepresentable = isJust . maybeUnrepresentableContent
