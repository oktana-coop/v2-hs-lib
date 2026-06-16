{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ProseMirror.Diff (toDecoratedPMDoc, DecoratedPMDoc) where

import Control.Monad (when)
import Control.Monad.State (State, evalState, get, modify)
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Maybe (isJust, listToMaybe)
import qualified Data.Text as T
import Data.Tree (Tree (..), foldTree)
import DocTree.Common as RichText (InlineSpan (..), TextSpan (..))
import qualified DocTree.LeafTextSpans as PandocTree
import ProseMirror.Decoration (Decoration (..), DecorationAttrs (..), InlineDecoration (..), NodeDecoration (..), WidgetDecoration (..), undecorate)
import qualified ProseMirror.Model as PM (InlineNode (..), Node (..), TextNode (..), isAtomNode, isLeafBlockNode, isRootBlockNode, wrapChildrenToBlock)
import ProseMirror.PandocTreeShape.FigureContent.LeafTextSpans (unwrapFigureContentParaOrPlain)
import ProseMirror.Tree (PMTreeNode (..), leafTextSpansPandocTreeNodeToPMNode, pmNodeFromInlineSpan, treeTextSpanNodeToPMTextNode)
import RichTextDiffOp (RichTextDiffOp (..), RichTextDiffOpType (InsertType, UpdateHeadingLevelType), getDiffOpType, unpackDiffOpValue)

-- Alias to the function exposed from the PMTree module
pandocTreeNodeToPMNode :: PandocTree.DocNode -> PMTreeNode
pandocTreeNodeToPMNode = leafTextSpansPandocTreeNodeToPMNode

type DecoratedPMTree = Tree (Either PMTreeNode (Decoration PMTreeNode))

data DecoratedPMDoc = DecoratedPMDoc {doc :: PM.Node, decorations :: [Decoration PM.Node]} deriving (Show, Eq)

instance ToJSON DecoratedPMDoc where
  toJSON decoratedPMDoc = object ["doc" .= doc decoratedPMDoc, "decorations" .= decorations decoratedPMDoc]

type PMIndex = Int

toDecoratedPMDoc :: Tree (RichTextDiffOp PandocTree.DocNode) -> DecoratedPMDoc
toDecoratedPMDoc = pmDocFromPMTree . toProseMirrorTreeWithDiffDecorations . unwrapFigureContentParaOrPlain unpackDiffOpValue

toProseMirrorTreeWithDiffDecorations :: Tree (RichTextDiffOp PandocTree.DocNode) -> DecoratedPMTree
toProseMirrorTreeWithDiffDecorations diffTree = evalState (walkDiffTree diffTree) 0

walkDiffTree :: Tree (RichTextDiffOp PandocTree.DocNode) -> State PMIndex DecoratedPMTree
walkDiffTree (Node nodeWithDiff subTrees) = do
  pmNode <- walkDiffTreeNode nodeWithDiff

  let maybeNonDeletedNode = getNonDeletedPMBlockNode pmNode
  let nonDeletedBlockNode = isJust maybeNonDeletedNode
  let isAtom = maybe False PM.isAtomNode maybeNonDeletedNode
  let isLeafBlock = maybe False PM.isLeafBlockNode maybeNonDeletedNode

  -- These conditional state updates are ugly.
  -- Unfortunately, ProseMirror indexing increases by 1 both before and after block nodes so I wasn't able to think of a good way to avoid it.

  -- Update state before processing children
  beforeNodeIndex <- get
  incrementIndexIf (nonDeletedBlockNode && not isAtom)

  -- Process children
  childTrees <- mapM (walkDiffTree) subTrees

  -- Update state after processing children
  incrementIndexIf (nonDeletedBlockNode && not isAtom && not isLeafBlock)
  afterNodeIndex <- get

  -- Build the node tree, conditionally adding a node decoration.
  -- The decoration for heading level updates (and any other op type that requires a node decoration)
  -- must be handled here because we need to know the size of the node (therefore, we need to have processed its subtree).
  -- Unfortunately, this is ugly; didn't think of a way to avoid it.
  case pmNode of
    Left undecoratedPMNode ->
      if mustWrapToNodeDecoration nodeWithDiff isLeafBlock
        then pure $ Node {rootLabel = Right $ decorateNode undecoratedPMNode beforeNodeIndex afterNodeIndex diffOpType, subForest = childTrees}
        else pure $ Node {rootLabel = pmNode, subForest = childTrees}
      where
        diffOpType = (getDiffOpType nodeWithDiff)
    Right _ -> pure $ Node {rootLabel = pmNode, subForest = childTrees}
  where
    incrementIndexIf :: Bool -> State PMIndex ()
    incrementIndexIf condition = when condition (modify (+ 1))

-- Inserted leaf blocks (e.g. horizontal rule) have no inline children to carry an insert
-- decoration, so we wrap the block itself in a node decoration instead.
mustWrapToNodeDecoration :: RichTextDiffOp PandocTree.DocNode -> Bool -> Bool
mustWrapToNodeDecoration (UpdateHeadingLevel _ _) _ = True
mustWrapToNodeDecoration (Insert _) isLeafBlock = isLeafBlock
mustWrapToNodeDecoration _ _ = False

diffInsertClass :: T.Text
diffInsertClass = "diff-insert"

diffModifyClass :: T.Text
diffModifyClass = "diff-modify"

decorateNode :: PMTreeNode -> PMIndex -> PMIndex -> RichTextDiffOpType -> Decoration PMTreeNode
decorateNode pmNode beforeNodeIndex afterNodeIndex InsertType =
  NodeDecoration $ wrapInNodeDecoration pmNode beforeNodeIndex afterNodeIndex diffInsertClass
decorateNode pmNode beforeNodeIndex afterNodeIndex UpdateHeadingLevelType =
  NodeDecoration $ wrapInNodeDecoration pmNode beforeNodeIndex afterNodeIndex diffModifyClass
decorateNode pmNode beforeNodeIndex afterNodeIndex _ = NodeDecoration $ wrapInNodeDecoration pmNode beforeNodeIndex afterNodeIndex diffModifyClass

walkDiffTreeNode :: RichTextDiffOp PandocTree.DocNode -> State PMIndex (Either PMTreeNode (Decoration PMTreeNode))
walkDiffTreeNode (Copy (PandocTree.TreeNode (PandocTree.InlineContent inlineSpan))) = walkInlineNode inlineSpan >>= pure . Left
-- Just transform non-text nodes to their PM equivalent (without decoration). For block nodes, increasing the index is handled in another function (`walkDiffTree`).
walkDiffTreeNode (Copy node) = pure $ Left $ pandocTreeNodeToPMNode node
walkDiffTreeNode (Insert (PandocTree.TreeNode (PandocTree.InlineContent inlineSpan))) = walkInlineNodeAddingDecoration inlineSpan diffInsertClass
walkDiffTreeNode (Insert node) = pure $ Left $ pandocTreeNodeToPMNode node
walkDiffTreeNode (Delete node) = do
  position <- get
  pure $ Right $ WidgetDecoration $ wrapInWidgetDecoration pmNode position
  where
    pmNode = pandocTreeNodeToPMNode node
-- We currently ignore meta diffs.
-- TODO: Handle meta diffs in ProseMirror.
walkDiffTreeNode (UpdateMeta _ node) = pure $ Left $ pandocTreeNodeToPMNode node
walkDiffTreeNode (UpdateMarks _ (PandocTree.TreeNode (PandocTree.InlineContent inlineSpan))) = walkInlineNodeAddingDecoration inlineSpan diffModifyClass
-- Just transform non-text nodes to their PM equivalent (without decoration).
-- We shouldn't really get this diff op for block nodes. TODO: Express this in the type system.
walkDiffTreeNode (UpdateMarks _ node) = pure $ Left $ pandocTreeNodeToPMNode node
-- The decoration for heading level updates is handled in `walkDiffTree` because
-- we need to know the size of the node (therefore, we need to have processed its subtree).
-- Unfortunately, this is ugly; didn't think of a way to avoid it.
-- So in this function we return the node undecorated.
walkDiffTreeNode (UpdateHeadingLevel _ node) = pure $ Left $ pandocTreeNodeToPMNode node

walkInlineNode :: InlineSpan -> State PMIndex PMTreeNode
walkInlineNode (InlineText textSpan) = walkTextNode textSpan
-- In ProseMirror nodes like note refs which don't have directly editable content and
-- should be treated as a single unit in the view have the `atom` property set to `true`.
-- https://prosemirror.net/docs/ref/#model.NodeSpec.atom
walkInlineNode inlineSpan = walkNodeMappingToPMAtom inlineSpan

walkNodeMappingToPMAtom :: InlineSpan -> State PMIndex PMTreeNode
walkNodeMappingToPMAtom inlineSpan = do
  modify (+ 1)
  pure pmNode
  where
    pmNode = pmNodeFromInlineSpan inlineSpan

walkTextNode :: TextSpan -> State PMIndex PMTreeNode
walkTextNode textSpan = do
  modify (+ textLength)
  pure pmNode
  where
    pmTextNode = treeTextSpanNodeToPMTextNode textSpan
    pmNode = PMNode $ PM.InlineNode $ PM.InlineText pmTextNode
    textLength = T.length $ PM.text pmTextNode

walkInlineNodeAddingDecoration :: InlineSpan -> T.Text -> State PMIndex (Either PMTreeNode (Decoration PMTreeNode))
walkInlineNodeAddingDecoration (InlineText textSpan) = walkTextNodeAddingDecoration textSpan
walkInlineNodeAddingDecoration inlineSpan = walkNodeMappingToPMAtomAddingDecoration inlineSpan

walkTextNodeAddingDecoration :: TextSpan -> T.Text -> State PMIndex (Either PMTreeNode (Decoration PMTreeNode))
walkTextNodeAddingDecoration textSpan cssClassName = do
  startIndex <- get
  pmNode <- walkTextNode textSpan
  endIndex <- get
  pure $ Right $ InlineDecoration $ wrapInInlineDecoration pmNode startIndex endIndex cssClassName

walkNodeMappingToPMAtomAddingDecoration :: InlineSpan -> T.Text -> State PMIndex (Either PMTreeNode (Decoration PMTreeNode))
walkNodeMappingToPMAtomAddingDecoration inlineSpan cssClassName = do
  startIndex <- get
  pmNode <- walkInlineNode inlineSpan
  endIndex <- get
  pure $ Right $ NodeDecoration $ wrapInNodeDecoration pmNode startIndex endIndex cssClassName

wrapInInlineDecoration :: PMTreeNode -> PMIndex -> PMIndex -> T.Text -> InlineDecoration PMTreeNode
wrapInInlineDecoration pmNode startIndex endIndex className =
  PMInlineDecoration
    { inlineDecFrom = startIndex,
      inlineDecTo = endIndex,
      inlineDecAttrs =
        DecorationAttrs
          { nodeName = Nothing,
            cssClass = Just className,
            style = Nothing
          },
      inlineDecContent = pmNode
    }

wrapInNodeDecoration :: PMTreeNode -> PMIndex -> PMIndex -> T.Text -> NodeDecoration PMTreeNode
wrapInNodeDecoration pmNode startIndex endIndex cssClassName =
  PMNodeDecoration
    { nodeDecFrom = startIndex,
      nodeDecTo = endIndex,
      nodeDecAttrs =
        DecorationAttrs
          { nodeName = Nothing,
            cssClass = Just cssClassName,
            style = Nothing
          },
      nodeDecContent = pmNode
    }

wrapInWidgetDecoration :: PMTreeNode -> PMIndex -> WidgetDecoration PMTreeNode
wrapInWidgetDecoration pmNode position =
  PMWidgetDecoration
    { pos = position,
      widgetDecContent = pmNode
    }

getNonDeletedPMBlockNode :: Either PMTreeNode (Decoration PMTreeNode) -> Maybe PM.Node
getNonDeletedPMBlockNode (Left (PMNode node@(PM.BlockNode _))) | not (PM.isRootBlockNode node) = Just node
getNonDeletedPMBlockNode (Left _) = Nothing
-- Inline decorations wrap text nodes, not block nodes.
-- TODO: Capture this properly in the type system.
getNonDeletedPMBlockNode (Right (InlineDecoration _)) = Nothing
-- Widget decorations capture deleted content in our case.
-- TODO: Capture this properly in the type system.
getNonDeletedPMBlockNode (Right (WidgetDecoration _)) = Nothing
getNonDeletedPMBlockNode (Right (NodeDecoration dec)) = case nodeDecContent dec of
  PMNode node@(PM.BlockNode _) | not (PM.isRootBlockNode node) -> Just node
  _ -> Nothing

pmDocFromPMTree :: DecoratedPMTree -> DecoratedPMDoc
pmDocFromPMTree pmTree = DecoratedPMDoc {doc = pmDoc, decorations = pmDecorations}
  where
    (pmDoc, pmDecorations) = extractRootBlock $ foldTree pmTreeNodeFolder pmTree

    extractRootBlock :: ([PM.Node], [Decoration PM.Node]) -> (PM.Node, [Decoration PM.Node])
    extractRootBlock (nodes, decs) = (assertRootNodeIsBlock $ listToMaybe nodes, decs)

    assertRootNodeIsBlock :: Maybe PM.Node -> PM.Node
    assertRootNodeIsBlock (Just n@(PM.BlockNode _)) = n
    -- TODO: Fail gracefully
    assertRootNodeIsBlock _ = undefined

pmTreeNodeFolder :: Either PMTreeNode (Decoration PMTreeNode) -> [([PM.Node], [Decoration PM.Node])] -> ([PM.Node], [Decoration PM.Node])
-- Undecorated inline node (text / image / note ref)
pmTreeNodeFolder (Left (PMNode pmNode@(PM.InlineNode _))) _ = ([pmNode], [])
-- Undecorated (wrapper) inline node
pmTreeNodeFolder (Left (WrapperInlineNode)) childNodesWithDecorations = splitNodesAndDecorations childNodesWithDecorations
-- Undecorated wrapper block node (div)
pmTreeNodeFolder (Left (WrapperBlockNode)) childNodesWithDecorations = splitNodesAndDecorations childNodesWithDecorations
-- Undecorated block node
pmTreeNodeFolder (Left (PMNode (PM.BlockNode blockNode))) childNodesWithDecorations = ([PM.BlockNode $ PM.wrapChildrenToBlock blockNode childNodes], childDecorations)
  where
    (childNodes, childDecorations) = splitNodesAndDecorations childNodesWithDecorations
-- Inline decoration for inline node (typically text).
-- TODO: See if making decoration a functor makes this case easier to write because in the second slot of the tuple we just want to map over the decoration structure.
pmTreeNodeFolder (Right (InlineDecoration (PMInlineDecoration decFrom decTo decAttrs (PMNode pmNode@(PM.InlineNode _))))) _ =
  ([pmNode], [InlineDecoration $ PMInlineDecoration decFrom decTo decAttrs pmNode])
-- Widget decoration for inline node. Don't add the content as a node (the content has been deleted), just add the widget decoration.
pmTreeNodeFolder (Right (WidgetDecoration (PMWidgetDecoration decPos (PMNode pmNode@(PM.InlineNode _))))) _ =
  ([], [WidgetDecoration $ PMWidgetDecoration decPos pmNode])
-- Widget decoration for wrapper inline node. Just return the children nodes and decorations (they will contain the decoration themselves)
pmTreeNodeFolder (Right (WidgetDecoration (PMWidgetDecoration _ (WrapperInlineNode)))) childNodesWithDecorations = splitNodesAndDecorations childNodesWithDecorations
-- Widget decoration for wrapper block node. Just return the children nodes and decorations (they will contain the decoration themselves)
pmTreeNodeFolder (Right (WidgetDecoration (PMWidgetDecoration _ (WrapperBlockNode)))) childNodesWithDecorations = splitNodesAndDecorations childNodesWithDecorations
-- Widget decoration for block node. Get the decorated child nodes, undecorate them and create a composite block decoration that includes all children.
-- In this case we ignore the node itself (return an empty list in the first slot of the tuple) since we only care about the decoration (the node is deleted).
pmTreeNodeFolder (Right (WidgetDecoration (PMWidgetDecoration decPos (PMNode (PM.BlockNode blockNode))))) childNodesWithDecorations =
  ([], [blockDecoration])
  where
    blockDecoration = WidgetDecoration $ PMWidgetDecoration decPos blockNodeWithChildren
    blockNodeWithChildren = PM.BlockNode $ PM.wrapChildrenToBlock blockNode $ map undecorate decoratedChildNodes
    (_, decoratedChildNodes) = splitNodesAndDecorations childNodesWithDecorations
-- Node decoration for block node
pmTreeNodeFolder (Right (NodeDecoration (PMNodeDecoration decFrom decTo decAttrs (PMNode (PM.BlockNode blockNode))))) childNodesWithDecorations =
  ([pmNode], [NodeDecoration $ PMNodeDecoration decFrom decTo decAttrs pmNode])
  where
    pmNode = PM.BlockNode $ PM.wrapChildrenToBlock blockNode childNodes
    (childNodes, _) = splitNodesAndDecorations childNodesWithDecorations
-- Node decoration for inline atom (image, note ref) — emitted by walkNodeMappingToPMAtomAddingDecoration.
pmTreeNodeFolder (Right (NodeDecoration (PMNodeDecoration decFrom decTo decAttrs (PMNode pmNode@(PM.InlineNode _))))) _ =
  ([pmNode], [NodeDecoration $ PMNodeDecoration decFrom decTo decAttrs pmNode])
-- TODO: There are cases we didn't handle, like an inline decoration wrapping blocks.
-- These are failure cases and we should guard against them, ideally in the type system (with more accurate/specific types).
pmTreeNodeFolder _ _ = undefined

splitNodesAndDecorations :: [([PM.Node], [Decoration PM.Node])] -> ([PM.Node], [Decoration PM.Node])
splitNodesAndDecorations nodesWithDecorations = (concatMap fst nodesWithDecorations, concatMap snd nodesWithDecorations)
