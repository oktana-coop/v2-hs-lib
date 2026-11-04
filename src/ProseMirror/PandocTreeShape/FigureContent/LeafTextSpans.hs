module ProseMirror.PandocTreeShape.FigureContent.LeafTextSpans (unwrapFigureContentParaOrPlain) where

import Data.Tree (Tree (..))
import qualified DocTree.Common as RichText
import qualified DocTree.LeafTextSpans as LeafTextSpansTree
import qualified Text.Pandoc.Builder as Pandoc (Block (..))

-- Strip the Plain/Para wrapper inside FigureContent (mapping a leaf-text-spans tree to another).
-- The mapping is defined given how to get the node from a label (a -> DocNode).
-- In practice we need this function in the diffing modules, where the tree's labels are diff nodes.
-- In the case of figure content (where we remove the node and spread its children), the removed node's label (e.g. its diff) is discarded too.
unwrapFigureContentParaOrPlain :: (a -> LeafTextSpansTree.DocNode) -> Tree a -> Tree a
unwrapFigureContentParaOrPlain extract = unwrapTree
  where
    unwrapTree (Node node children)
      | isFigureContent (extract node) = Node node (concatMap spreadIfPlainOrPara children)
      | otherwise = Node node (map unwrapTree children)

    spreadIfPlainOrPara nodeTree@(Node n ch)
      | isPlainOrPara (extract n) = map unwrapTree ch
      | otherwise = [unwrapTree nodeTree]

    isFigureContent (LeafTextSpansTree.RichTextNode (LeafTextSpansTree.BlockNode (RichText.FigureContent _))) = True
    isFigureContent _ = False

    isPlainOrPara (LeafTextSpansTree.RichTextNode (LeafTextSpansTree.BlockNode (RichText.PandocBlock (Pandoc.Plain _)))) = True
    isPlainOrPara (LeafTextSpansTree.RichTextNode (LeafTextSpansTree.BlockNode (RichText.PandocBlock (Pandoc.Para _)))) = True
    isPlainOrPara _ = False
