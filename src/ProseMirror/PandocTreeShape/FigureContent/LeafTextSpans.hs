{-# LANGUAGE RankNTypes #-}

module ProseMirror.PandocTreeShape.FigureContent.LeafTextSpans (unwrapFigureContentParaOrPlain) where

import Data.Tree (Tree (..))
import qualified DocTree.Common as RichText
import qualified DocTree.LeafTextSpans as LeafTextSpansTree
import qualified Text.Pandoc.Builder as Pandoc (Block (..))

-- Strip the Plain/Para wrapper inside FigureContent (mapping a leaf-text-spans tree to another).
-- The mapping is defined given an extractor function (w x -> x).
-- In practice we need this function in the diffing modules, where tree nodes are wrapped in the RichTextDiffOp structure.
-- In the case of figure content (where we remove the node and spread its children), the wrapping structure (e.g. diff op) is discarded too.
unwrapFigureContentParaOrPlain :: (forall x. w x -> x) -> Tree (w LeafTextSpansTree.DocNode) -> Tree (w LeafTextSpansTree.DocNode)
unwrapFigureContentParaOrPlain extract = unwrapTree
  where
    unwrapTree (Node node children)
      | isFigureContent (extract node) = Node node (concatMap spreadIfPlainOrPara children)
      | otherwise = Node node (map unwrapTree children)

    spreadIfPlainOrPara nodeTree@(Node n ch)
      | isPlainOrPara (extract n) = map unwrapTree ch
      | otherwise = [unwrapTree nodeTree]

    isFigureContent (LeafTextSpansTree.TreeNode (LeafTextSpansTree.BlockNode (RichText.FigureContent _))) = True
    isFigureContent _ = False

    isPlainOrPara (LeafTextSpansTree.TreeNode (LeafTextSpansTree.BlockNode (RichText.PandocBlock (Pandoc.Plain _)))) = True
    isPlainOrPara (LeafTextSpansTree.TreeNode (LeafTextSpansTree.BlockNode (RichText.PandocBlock (Pandoc.Para _)))) = True
    isPlainOrPara _ = False
