{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ProseMirror.Diff (toDecoratedPMDoc, DecoratedPMDoc) where

import Control.Monad (when)
import Control.Monad.State (State, evalState, get, modify)
import Data.Aeson (ToJSON, Value (Number, String), object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.Tree (Tree (..), drawTree, foldTree)
import qualified Debug.Trace
import DocTree.Common as RichText (BlockNode (..), LinkMark (..), Mark (..), TextSpan (..))
import DocTree.LeafTextSpans as PandocTree (DocNode (..), TreeNode (..))
import ProseMirror.Decoration (Decoration (..), DecorationAttrs (..), InlineDecoration (..), NodeDecoration (..), WidgetDecoration (..), undecorate)
import ProseMirror.PMJson (Mark (markAttrs), isRootBlockNode)
import qualified ProseMirror.PMJson as PM (BlockNode (..), Mark (..), Node (..), TextNode (..))
import RichTextDiffOp (RichTextDiffOp (..), RichTextDiffOpType (UpdateHeadingLevelType), getDiffOpType)
import Text.Pandoc.Definition as Pandoc (Block (..))

data PMTreeNode = PMNode PM.Node | WrapperInlineNode | WrapperBlockNode deriving (Show)

type DecoratedPMTree = Tree (Either PMTreeNode (Decoration PMTreeNode))

data DecoratedPMDoc = DecoratedPMDoc {doc :: PM.BlockNode, decorations :: [Decoration PM.Node]} deriving (Show, Eq)

instance ToJSON DecoratedPMDoc where
  toJSON decoratedPMDoc = object ["doc" .= doc decoratedPMDoc, "decorations" .= decorations decoratedPMDoc]

type PMIndex = Int

toDecoratedPMDoc :: Tree (RichTextDiffOp PandocTree.DocNode) -> DecoratedPMDoc
toDecoratedPMDoc = pmDocFromPMTree . traceTree . toProseMirrorTreeWithDiffDecorations

traceTree :: (Show a) => Tree a -> Tree a
traceTree tree = Debug.Trace.trace (drawTree $ fmap show tree) tree

toProseMirrorTreeWithDiffDecorations :: Tree (RichTextDiffOp PandocTree.DocNode) -> DecoratedPMTree
toProseMirrorTreeWithDiffDecorations diffTree = evalState (walkDiffTree diffTree) 0

walkDiffTree :: Tree (RichTextDiffOp PandocTree.DocNode) -> State PMIndex DecoratedPMTree
walkDiffTree (Node nodeWithDiff subTrees) = do
  pmNode <- walkDiffTreeNode nodeWithDiff
  notDeletedBlockNode <- pure $ isNotDeletedPMBlockNode pmNode

  -- These conditional state updates are ugly.
  -- Unfortunately, ProseMirror indexing increases by 1 both before and after block nodes so I wasn't able to think of a good way to avoid it.

  -- Update state before processing children
  beforeNodeIndex <- get
  incrementIndexIf notDeletedBlockNode

  -- Process children
  childTrees <- mapM (walkDiffTree) subTrees

  -- Update state after processing children
  incrementIndexIf notDeletedBlockNode
  afterNodeIndex <- get

  -- Build the node tree, conditionally adding a node decoration.
  -- The decoration for heading level updates (and any other op type that requires a node decoration)
  -- must be handled here because we need to know the size of the node (therefore, we need to have processed its subtree).
  -- Unfortunately, this is ugly; didn't think of a way to avoid it.
  case pmNode of
    Left undecoratedPMNode ->
      if mustWrapToNodeDecoration nodeWithDiff
        then pure $ Node {rootLabel = Right $ decorateNode undecoratedPMNode beforeNodeIndex afterNodeIndex diffOpType, subForest = childTrees}
        else pure $ Node {rootLabel = pmNode, subForest = childTrees}
      where
        diffOpType = (getDiffOpType nodeWithDiff)
    Right _ -> pure $ Node {rootLabel = pmNode, subForest = childTrees}
  where
    incrementIndexIf :: Bool -> State PMIndex ()
    incrementIndexIf condition = when condition (modify (+ 1))

mustWrapToNodeDecoration :: RichTextDiffOp PandocTree.DocNode -> Bool
mustWrapToNodeDecoration (UpdateHeadingLevel _ _) = True
mustWrapToNodeDecoration _ = False

decorateNode :: PMTreeNode -> PMIndex -> PMIndex -> RichTextDiffOpType -> Decoration PMTreeNode
decorateNode pmNode beforeNodeIndex afterNodeIndex UpdateHeadingLevelType =
  NodeDecoration $ wrapInNodeDecoration pmNode beforeNodeIndex afterNodeIndex "bg-purple-100"
decorateNode pmNode beforeNodeIndex afterNodeIndex _ = NodeDecoration $ wrapInNodeDecoration pmNode beforeNodeIndex afterNodeIndex "bg-purple-100"

walkDiffTreeNode :: RichTextDiffOp PandocTree.DocNode -> State PMIndex (Either PMTreeNode (Decoration PMTreeNode))
walkDiffTreeNode (Copy (PandocTree.TreeNode (InlineContent textSpan))) = walkTextNode textSpan >>= pure . Left
-- Just transform non-text nodes to their PM equivalent (without decoration). For block nodes, increasing the index is handled in another function (`walkDiffTree`).
walkDiffTreeNode (Copy node) = pure $ Left $ docNodeToPMNode node
walkDiffTreeNode (Insert (PandocTree.TreeNode (InlineContent textSpan))) = walkTextNodeAddingDecoration textSpan "bg-green-300"
walkDiffTreeNode (Insert node) = pure $ Left $ docNodeToPMNode node
walkDiffTreeNode (Delete node) = do
  position <- get
  pure $ Right $ WidgetDecoration $ wrapInWidgetDecoration pmNode position
  where
    pmNode = docNodeToPMNode node
walkDiffTreeNode (UpdateMarks _ (PandocTree.TreeNode (InlineContent textSpan))) = walkTextNodeAddingDecoration textSpan "bg-purple-100"
-- Just transform non-text nodes to their PM equivalent (without decoration).
-- We shouldn't really get this diff op for block nodes. TODO: Express this in the type system.
walkDiffTreeNode (UpdateMarks _ node) = pure $ Left $ docNodeToPMNode node
-- The decoration for heading level updates is handled in `walkDiffTree` because
-- we need to know the size of the node (therefore, we need to have processed its subtree).
-- Unfortunately, this is ugly; didn't think of a way to avoid it.
-- So in this function we return the node undecorated.
walkDiffTreeNode (UpdateHeadingLevel _ node) = pure $ Left $ docNodeToPMNode node

walkTextNodeAddingDecoration :: TextSpan -> T.Text -> State PMIndex (Either PMTreeNode (Decoration PMTreeNode))
walkTextNodeAddingDecoration textSpan cssClassName = do
  startIndex <- get
  pmNode <- walkTextNode textSpan
  endIndex <- get
  pure $ Right $ InlineDecoration $ wrapInInlineDecoration pmNode startIndex endIndex cssClassName

walkTextNode :: TextSpan -> State PMIndex PMTreeNode
walkTextNode textSpan = do
  modify (+ textLength)
  pure pmNode
  where
    pmTextNode = treeTextSpanNodeToPMTextNode textSpan
    pmNode = PMNode $ PM.TextNode $ pmTextNode
    textLength = T.length $ PM.text pmTextNode

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

docNodeToPMNode :: PandocTree.DocNode -> PMTreeNode
docNodeToPMNode Root = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "doc", PM.content = Nothing, PM.attrs = Nothing}
docNodeToPMNode (PandocTree.TreeNode (BlockNode node)) = treeBlockNodeToPMBlockNode node
docNodeToPMNode (PandocTree.TreeNode (InlineNode)) = WrapperInlineNode
docNodeToPMNode (PandocTree.TreeNode (InlineContent textSpan)) = PMNode $ PM.TextNode $ treeTextSpanNodeToPMTextNode textSpan

-- TODO: Use ProseMirror schema as a parameter
treeBlockNodeToPMBlockNode :: RichText.BlockNode -> PMTreeNode
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Plain _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "paragraph", PM.content = Nothing, PM.attrs = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Para _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "paragraph", PM.content = Nothing, PM.attrs = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Header level _ _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "heading", PM.content = Nothing, PM.attrs = Just $ KM.fromList [(K.fromText "level", Number (fromIntegral level))]}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.CodeBlock _ _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "code_block", PM.content = Nothing, PM.attrs = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.BulletList _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "bullet_list", PM.content = Nothing, PM.attrs = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.OrderedList _ _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "ordered_list", PM.content = Nothing, PM.attrs = Nothing}
treeBlockNodeToPMBlockNode (RichText.ListItem _) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "list_item", PM.content = Nothing, PM.attrs = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Div _ _)) = WrapperBlockNode
-- TODO: Incrementally handle more blocks
treeBlockNodeToPMBlockNode _ = undefined

-- TODO: Use ProseMirror schema as a parameter
treeTextSpanNodeToPMTextNode :: RichText.TextSpan -> PM.TextNode
treeTextSpanNodeToPMTextNode textSpan = PM.PMText {PM.text = value textSpan, PM.marks = (fmap . fmap) toPMMark (nonEmpty $ marks textSpan)}
  where
    toPMMark :: RichText.Mark -> PM.Mark
    toPMMark RichText.EmphMark = PM.PMMark {PM.markType = "em", markAttrs = Nothing}
    toPMMark RichText.StrongMark = PM.PMMark {PM.markType = "strong", markAttrs = Nothing}
    toPMMark (RichText.LinkMark (RichText.Link _ (linkUrl, linkTitle))) = PM.PMMark {PM.markType = "link", markAttrs = Just $ KM.fromList [(K.fromText "href", String linkUrl), (K.fromText "title", String linkTitle)]}

isNotDeletedPMBlockNode :: Either PMTreeNode (Decoration PMTreeNode) -> Bool
isNotDeletedPMBlockNode (Left (PMNode (PM.BlockNode blockNode))) = not (isRootBlockNode blockNode)
isNotDeletedPMBlockNode (Left _) = False
-- Inline decorations wrap text nodes, not block nodes.
-- TODO: Capture this properly in the type system.
isNotDeletedPMBlockNode (Right (InlineDecoration _)) = False
-- Widget decorations capture deleted content in our case.
-- TODO: Capture this properly in the type system.
isNotDeletedPMBlockNode (Right (WidgetDecoration _)) = False
isNotDeletedPMBlockNode (Right (NodeDecoration dec)) = case nodeDecContent dec of
  PMNode (PM.BlockNode blockNode) -> not (isRootBlockNode blockNode)
  _ -> False

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
-- Undecorated wrapper block node (div)
pmTreeNodeFolder (Left (WrapperBlockNode)) childNodesWithDecorations = splitNodesAndDecorations childNodesWithDecorations
-- Undecorated block node
pmTreeNodeFolder (Left (PMNode (PM.BlockNode blockNode))) childNodesWithDecorations = ([PM.BlockNode $ wrapChildrenToBlock blockNode childNodes], childDecorations)
  where
    (childNodes, childDecorations) = splitNodesAndDecorations childNodesWithDecorations
-- Inline decoration for text node.
-- TODO: See if making decoration a functor makes this case easier to write because in the second slot of the tuple we just want to map over the decoration structure.
pmTreeNodeFolder (Right (InlineDecoration (PMInlineDecoration decFrom decTo decAttrs (PMNode pmNode@(PM.TextNode _))))) _ =
  ([pmNode], [InlineDecoration $ PMInlineDecoration decFrom decTo decAttrs pmNode])
-- Widget decoration for text node. Don't add the content as a node (the content has been deleted), just add the widget decoration.
pmTreeNodeFolder (Right (WidgetDecoration (PMWidgetDecoration decPos (PMNode pmNode@(PM.TextNode _))))) _ =
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
    blockNodeWithChildren = PM.BlockNode $ wrapChildrenToBlock blockNode $ map undecorate decoratedChildNodes
    (_, decoratedChildNodes) = splitNodesAndDecorations childNodesWithDecorations
-- Node decoration
pmTreeNodeFolder (Right (NodeDecoration (PMNodeDecoration decFrom decTo decAttrs (PMNode (PM.BlockNode blockNode))))) childNodesWithDecorations =
  ([pmNode], [NodeDecoration $ PMNodeDecoration decFrom decTo decAttrs pmNode])
  where
    pmNode = PM.BlockNode $ wrapChildrenToBlock blockNode childNodes
    (childNodes, _) = splitNodesAndDecorations childNodesWithDecorations
-- TODO: There are cases we didn't handle, like an inline decoration wrapping blocks.
-- These are failure cases and we should guard against them, ideally in the type system (with more accurate/specific types).
pmTreeNodeFolder _ _ = undefined

wrapChildrenToBlock :: PM.BlockNode -> [PM.Node] -> PM.BlockNode
wrapChildrenToBlock (PM.PMBlock blockType Nothing blockAttrs) children = PM.PMBlock blockType (Just children) blockAttrs
wrapChildrenToBlock (PM.PMBlock blockType (Just existingChildren) blockAttrs) newChildren = PM.PMBlock blockType (Just (existingChildren <> newChildren)) blockAttrs

splitNodesAndDecorations :: [([PM.Node], [Decoration PM.Node])] -> ([PM.Node], [Decoration PM.Node])
splitNodesAndDecorations nodesWithDecorations = (concatMap fst nodesWithDecorations, concatMap snd nodesWithDecorations)
