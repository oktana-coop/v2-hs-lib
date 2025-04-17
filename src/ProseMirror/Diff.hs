{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module ProseMirror.Diff (proseMirrorDocWithDiffDecorations) where

import Control.Monad.State (State, evalState, modify)
import qualified Data.Text as T
import Data.Tree (Tree (..))
import DocTree.Common (BlockNode, TextSpan (..))
import DocTree.LeafTextSpans (DocNode (..), TreeNode (..))
import qualified ProseMirror.PMJson as PM (BlockNode (..), Node (..), TextNode)
import RichTextDiffOp (RichTextDiffOp (..))

data DecorationAttrs = DecorationAttrs {nodeName :: Maybe T.Text, cssClass :: Maybe T.Text, style :: Maybe T.Text} deriving (Show, Eq)

data InlineDecoration a = PMInlineDecoration {from :: Int, to :: Int, attrs :: DecorationAttrs} deriving (Show, Eq)

data WidgetDecoration a = PMWidgetDecoration {pos :: Int} deriving (Show, Eq)

data Decoration a = InlineDecoration (InlineDecoration a) | WidgetDecoration (WidgetDecoration a) deriving (Show, Eq)

data PMTreeNode = PMNode PM.Node | WrapperInlineNode

type DecoratedPMTree = Tree (Either PMTreeNode (Decoration PMTreeNode))

type PMIndex = Int

proseMirrorDocWithDiffDecorations :: Tree (RichTextDiffOp DocNode) -> DecoratedPMTree
proseMirrorDocWithDiffDecorations diffTree = evalState (walkDiffTree diffTree) 0

walkDiffTree :: Tree (RichTextDiffOp DocNode) -> State PMIndex DecoratedPMTree
walkDiffTree (Node nodeWithDiff subTrees) = do
  pmNode <- walkDiffTreeNode nodeWithDiff
  notDeletedBlockNode <- pure $ isNotDeletedPMBlockNode pmNode
  -- This is ugly. Unfortunately, ProseMirror indexing increases by 1 at the end of block nodes (after processing the subtrees),
  -- so I wasn't able to think of a good way to avoid it. The else case is a noop.
  if notDeletedBlockNode then modify (+ 1) else pure ()
  pmSubForest <- mapM (walkDiffTree) subTrees
  if notDeletedBlockNode then modify (+ 1) else pure ()
  pure $ Node {rootLabel = pmNode, subForest = pmSubForest}

walkDiffTreeNode :: RichTextDiffOp DocNode -> State PMIndex (Either PMTreeNode (Decoration PMTreeNode))
walkDiffTreeNode (Copy node) = do
  pmNode <- docNodeToPMNode node
  pure $ Left pmNode
walkDiffTreeNode _ = undefined

docNodeToPMNode :: DocNode -> State PMIndex PMTreeNode
docNodeToPMNode Root = pure $ PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "doc", PM.content = Nothing, PM.attrs = Nothing}
docNodeToPMNode (TreeNode (BlockNode node)) = (pure . PMNode . PM.BlockNode) pmNode
  where
    pmNode = treeBlockNodeToPMBlockNode node
docNodeToPMNode (TreeNode (InlineNode)) = pure $ WrapperInlineNode
docNodeToPMNode (TreeNode (InlineContent textSpan)) = do
  modify (+ len)
  (pure . PMNode . PM.TextNode) pmTextNode
  where
    (pmTextNode, len) = treeTextSpanNodeToPMTextNode textSpan

treeBlockNodeToPMBlockNode :: BlockNode -> PM.BlockNode
treeBlockNodeToPMBlockNode = undefined

treeTextSpanNodeToPMTextNode :: TextSpan -> (PM.TextNode, Int)
treeTextSpanNodeToPMTextNode = undefined

isNotDeletedPMBlockNode :: Either PMTreeNode (Decoration PMTreeNode) -> Bool
isNotDeletedPMBlockNode (Left (PMNode (PM.BlockNode _))) = True
isNotDeletedPMBlockNode (Left _) = False
-- Inline decorations wrap text nodes, not block nodes.
-- TODO: Capture this properly in the type system.
isNotDeletedPMBlockNode (Right (InlineDecoration _)) = False
-- Widget decorations capture deleted content in our case.
-- TODO: Capture this properly in the type system.
isNotDeletedPMBlockNode (Right (WidgetDecoration _)) = False
