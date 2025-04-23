{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ProseMirror.Diff (proseMirrorJSONDocWithDiffDecorations) where

import Control.Monad.State (State, evalState, get, modify)
import Data.Aeson (ToJSON, encode, toJSON)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Tree (Tree (..), foldTree)
import DocTree.Common (BlockNode, TextSpan (..))
import DocTree.LeafTextSpans (DocNode (..), TreeNode (..))
import qualified ProseMirror.PMJson as PM (BlockNode (..), Node (..), TextNode (..))
import RichTextDiffOp (RichTextDiffOp (..))

data DecorationAttrs = DecorationAttrs {nodeName :: Maybe T.Text, cssClass :: Maybe T.Text, style :: Maybe T.Text} deriving (Show, Eq)

data InlineDecoration a = PMInlineDecoration {from :: Int, to :: Int, attrs :: DecorationAttrs, decoratedInline :: a} deriving (Show, Eq)

data WidgetDecoration a = PMWidgetDecoration {pos :: Int, decoratedNode :: a} deriving (Show, Eq)

data Decoration a = InlineDecoration (InlineDecoration a) | WidgetDecoration (WidgetDecoration a) deriving (Show, Eq)

data PMTreeNode = PMNode PM.Node | WrapperInlineNode

type DecoratedPMTree = Tree (Either PMTreeNode (Decoration PMTreeNode))

data DecoratedPMDoc = DecoratedPMDoc {doc :: PM.BlockNode, decorations :: [Decoration PM.Node]} deriving (Show, Eq)

instance ToJSON DecoratedPMDoc where
  toJSON pmDoc = undefined

type PMIndex = Int

proseMirrorJSONDocWithDiffDecorations :: Tree (RichTextDiffOp DocNode) -> T.Text
proseMirrorJSONDocWithDiffDecorations = decodeUtf8 . BSL8.toStrict . encode . pmDocFromPMTree . toProseMirrorTreeWithDiffDecorations

pmDocFromPMTree :: DecoratedPMTree -> DecoratedPMDoc
pmDocFromPMTree pmTree = DecoratedPMDoc {doc = pmDoc, decorations = pmDecorations}
  where
    (pmDoc, pmDecorations) = extractRootBlock $ foldTree pmTreeNodeFolder pmTree

    extractRootBlock :: ([PM.Node], [Decoration PM.Node]) -> (PM.BlockNode, [Decoration PM.Node])
    extractRootBlock (nodes, decs) = (assertRootNodeIsBlock $ listToMaybe nodes, decs)

    assertRootNodeIsBlock :: Maybe PM.Node -> PM.BlockNode
    assertRootNodeIsBlock (Just (PM.BlockNode rootNode)) = rootNode
    -- TODO: Fail gracefully
    assertRootNodeIsBlock _ = undefined

pmTreeNodeFolder :: Either PMTreeNode (Decoration PMTreeNode) -> [([PM.Node], [Decoration PM.Node])] -> ([PM.Node], [Decoration PM.Node])
-- Undecorated text node
pmTreeNodeFolder (Left (PMNode pmNode@(PM.TextNode _))) _ = ([pmNode], [])
-- Undecorated (wrapper) inline node
pmTreeNodeFolder (Left (WrapperInlineNode)) childNodesWithDecorations = splitNodesAndDecorations childNodesWithDecorations
pmTreeNodeFolder (Left (PMNode (PM.BlockNode blockNode))) childNodesWithDecorations = ([PM.BlockNode $ wrapChildrenToBlock blockNode childNodes], childDecorations)
  where
    (childNodes, childDecorations) = splitNodesAndDecorations childNodesWithDecorations
pmTreeNodeFolder _ _ = undefined

wrapChildrenToBlock :: PM.BlockNode -> [PM.Node] -> PM.BlockNode
wrapChildrenToBlock (PM.PMBlock blockType _ blockAttrs) children = PM.PMBlock blockType (Just children) blockAttrs

splitNodesAndDecorations :: [([PM.Node], [Decoration PM.Node])] -> ([PM.Node], [Decoration PM.Node])
splitNodesAndDecorations nodesWithDecorations = (concatMap fst nodesWithDecorations, concatMap snd nodesWithDecorations)

toProseMirrorTreeWithDiffDecorations :: Tree (RichTextDiffOp DocNode) -> DecoratedPMTree
toProseMirrorTreeWithDiffDecorations diffTree = evalState (walkDiffTree diffTree) 0

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
walkDiffTreeNode (Copy (TreeNode (InlineContent textSpan))) = do
  modify (+ textLength)
  pure $ Left pmNode
  where
    pmTextNode = treeTextSpanNodeToPMTextNode textSpan
    pmNode = PMNode $ PM.TextNode $ pmTextNode
    textLength = T.length $ PM.text pmTextNode
-- Just transform non-text nodes to their PM equivalent (without decoration). For block nodes, increasing the index is handled in another function (`walkDiffTree`).
walkDiffTreeNode (Copy node) = pure $ Left $ docNodeToPMNode node
walkDiffTreeNode (Insert (TreeNode (InlineContent textSpan))) = do
  startIndex <- get
  modify (+ textLength)
  pure $ Right $ InlineDecoration $ wrapInInlineDecoration pmNode startIndex (startIndex + textLength) "diff-insert"
  where
    pmTextNode = treeTextSpanNodeToPMTextNode textSpan
    pmNode = PMNode $ PM.TextNode $ pmTextNode
    textLength = T.length $ PM.text pmTextNode
walkDiffTreeNode (Insert node) = pure $ Left $ docNodeToPMNode node
walkDiffTreeNode (Delete node) = do
  position <- get
  pure $ Right $ WidgetDecoration $ wrapInWidgetDecoration pmNode position
  where
    pmNode = docNodeToPMNode node
walkDiffTreeNode (UpdateMarks _ (TreeNode (InlineContent textSpan))) = do
  startIndex <- get
  modify (+ textLength)
  pure $ Right $ InlineDecoration $ wrapInInlineDecoration pmNode startIndex (startIndex + textLength) "diff-modify"
  where
    pmTextNode = treeTextSpanNodeToPMTextNode textSpan
    pmNode = PMNode $ PM.TextNode $ pmTextNode
    textLength = T.length $ PM.text pmTextNode
-- Just transform non-text nodes to their PM equivalent (without decoration).
-- We shouldn't really get this diff op for block nodes. TODO: Express this in the type system.
walkDiffTreeNode (UpdateMarks _ node) = pure $ Left $ docNodeToPMNode node
-- TODO: Implement heading level updates properly
walkDiffTreeNode (UpdateHeadingLevel _ node) = pure $ Left $ docNodeToPMNode node

wrapInInlineDecoration :: PMTreeNode -> PMIndex -> PMIndex -> T.Text -> InlineDecoration PMTreeNode
wrapInInlineDecoration pmNode startIndex endIndex cssClassName =
  PMInlineDecoration
    { from = startIndex,
      to = endIndex,
      attrs = DecorationAttrs {nodeName = Nothing, cssClass = Just cssClassName, style = Nothing},
      decoratedInline = pmNode
    }

wrapInWidgetDecoration :: PMTreeNode -> PMIndex -> WidgetDecoration PMTreeNode
wrapInWidgetDecoration pmNode position =
  PMWidgetDecoration
    { pos = position,
      decoratedNode = pmNode
    }

docNodeToPMNode :: DocNode -> PMTreeNode
docNodeToPMNode Root = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "doc", PM.content = Nothing, PM.attrs = Nothing}
docNodeToPMNode (TreeNode (BlockNode node)) = PMNode $ PM.BlockNode $ treeBlockNodeToPMBlockNode node
docNodeToPMNode (TreeNode (InlineNode)) = WrapperInlineNode
docNodeToPMNode (TreeNode (InlineContent textSpan)) = PMNode $ PM.TextNode $ treeTextSpanNodeToPMTextNode textSpan

treeBlockNodeToPMBlockNode :: BlockNode -> PM.BlockNode
treeBlockNodeToPMBlockNode = undefined

treeTextSpanNodeToPMTextNode :: TextSpan -> PM.TextNode
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
