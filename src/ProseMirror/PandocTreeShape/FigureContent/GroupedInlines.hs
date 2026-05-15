-- ProseMirror's `figure_content { image? }` schema accepts the image as a direct inline
-- child, whereas Pandoc's canonical figure body wraps the image in a `Plain` (or `Para`) block.
-- These adapters bridge the gap at the v2-hs-lib boundary, leaving pandoc-tree itself neutral.
module ProseMirror.PandocTreeShape.FigureContent.GroupedInlines
  ( unwrapFigureContentParaOrPlain,
    wrapFigureContentInlinesToPlain,
  )
where

import Data.Tree (Tree (..))
import qualified DocTree.Common as RichText
import qualified DocTree.GroupedInlines as GroupedInlinesTree
import qualified Text.Pandoc.Builder as Pandoc (Block (..))

-- Strip the Plain/Para wrapper inside FigureContent.
unwrapFigureContentParaOrPlain :: Tree GroupedInlinesTree.DocNode -> Tree GroupedInlinesTree.DocNode
unwrapFigureContentParaOrPlain (Node node children)
  | isFigureContent node = Node node (concatMap spreadIfPlainOrPara children)
  | otherwise = Node node (map unwrapFigureContentParaOrPlain children)
  where
    spreadIfPlainOrPara :: Tree GroupedInlinesTree.DocNode -> [Tree GroupedInlinesTree.DocNode]
    spreadIfPlainOrPara nodeTree@(Node n ch)
      | isPlainOrPara n = map unwrapFigureContentParaOrPlain ch
      | otherwise = [unwrapFigureContentParaOrPlain nodeTree]

    isPlainOrPara (GroupedInlinesTree.TreeNode (GroupedInlinesTree.BlockNode (RichText.PandocBlock (Pandoc.Plain _)))) = True
    isPlainOrPara (GroupedInlinesTree.TreeNode (GroupedInlinesTree.BlockNode (RichText.PandocBlock (Pandoc.Para _)))) = True
    isPlainOrPara _ = False

-- Insert a `Plain []` block between FigureContent and its inline child.
wrapFigureContentInlinesToPlain :: Tree GroupedInlinesTree.DocNode -> Tree GroupedInlinesTree.DocNode
wrapFigureContentInlinesToPlain (Node node children)
  | isFigureContent node = Node node (map wrapIfInline children)
  | otherwise = Node node (map wrapFigureContentInlinesToPlain children)
  where
    wrapIfInline nodeTree@(Node n _)
      | isInline n = Node plainBlockNode [wrapFigureContentInlinesToPlain nodeTree]
      | otherwise = wrapFigureContentInlinesToPlain nodeTree

    plainBlockNode :: GroupedInlinesTree.DocNode
    plainBlockNode = GroupedInlinesTree.TreeNode (GroupedInlinesTree.BlockNode (RichText.PandocBlock (Pandoc.Plain [])))

isFigureContent :: GroupedInlinesTree.DocNode -> Bool
isFigureContent (GroupedInlinesTree.TreeNode (GroupedInlinesTree.BlockNode (RichText.FigureContent _))) = True
isFigureContent _ = False

isInline :: GroupedInlinesTree.DocNode -> Bool
isInline (GroupedInlinesTree.TreeNode (GroupedInlinesTree.InlineNode _)) = True
isInline _ = False