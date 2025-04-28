{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ProseMirror.Diff (proseMirrorJSONDocWithDiffDecorations, toDecoratedPMDoc, DecoratedPMDoc) where

import Control.Monad.State (State, evalState, get, modify)
import Data.Aeson (ToJSON, Value (Number, String), encode, object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Tree (Tree (..), foldTree)
import DocTree.Common as RichText (BlockNode (..), LinkMark (..), Mark (..), TextSpan (..))
import DocTree.LeafTextSpans (DocNode (..), TreeNode (..))
import ProseMirror.PMJson (Mark (markAttrs), isRootBlockNode)
import qualified ProseMirror.PMJson as PM (BlockNode (..), Mark (..), Node (..), TextNode (..))
import RichTextDiffOp (RichTextDiffOp (..))
import Text.Pandoc.Definition as Pandoc (Block (..))

data DecorationAttrs = DecorationAttrs {nodeName :: Maybe T.Text, cssClass :: Maybe T.Text, style :: Maybe T.Text} deriving (Show, Eq)

instance ToJSON DecorationAttrs where
  toJSON decAttrs = object ["nodeName" .= nodeName decAttrs, "class" .= cssClass decAttrs, "style" .= style decAttrs]

data InlineDecoration a = PMInlineDecoration {from :: Int, to :: Int, attrs :: DecorationAttrs, decoratedInline :: a} deriving (Show, Eq)

instance ToJSON (InlineDecoration a) where
  toJSON dec = object ["type" .= T.pack "inline", "from" .= from dec, "to" .= to dec, "attrs" .= attrs dec]

data WidgetDecoration a = PMWidgetDecoration {pos :: Int, decoratedNode :: a} deriving (Show, Eq)

instance (ToJSON a) => ToJSON (WidgetDecoration a) where
  toJSON dec = object ["type" .= T.pack "widget", "pos" .= pos dec, "node" .= toJSON (decoratedNode dec)]

data Decoration a = InlineDecoration (InlineDecoration a) | WidgetDecoration (WidgetDecoration a) deriving (Show, Eq)

instance (ToJSON a) => ToJSON (Decoration a) where
  toJSON (InlineDecoration inlineDec) = toJSON inlineDec
  toJSON (WidgetDecoration widgetDec) = toJSON widgetDec

data PMTreeNode = PMNode PM.Node | WrapperInlineNode

type DecoratedPMTree = Tree (Either PMTreeNode (Decoration PMTreeNode))

data DecoratedPMDoc = DecoratedPMDoc {doc :: PM.BlockNode, decorations :: [Decoration PM.Node]} deriving (Show, Eq)

instance ToJSON DecoratedPMDoc where
  toJSON decoratedPMDoc = object ["doc" .= doc decoratedPMDoc, "decorations" .= decorations decoratedPMDoc]

type PMIndex = Int

proseMirrorJSONDocWithDiffDecorations :: Tree (RichTextDiffOp DocNode) -> T.Text
proseMirrorJSONDocWithDiffDecorations = decodeUtf8 . BSL8.toStrict . encode . pmDocFromPMTree . toProseMirrorTreeWithDiffDecorations

toDecoratedPMDoc :: Tree (RichTextDiffOp DocNode) -> DecoratedPMDoc
toDecoratedPMDoc = pmDocFromPMTree . toProseMirrorTreeWithDiffDecorations

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
-- Undecorated block node
pmTreeNodeFolder (Left (PMNode (PM.BlockNode blockNode))) childNodesWithDecorations = ([PM.BlockNode $ wrapChildrenToBlock blockNode childNodes], childDecorations)
  where
    (childNodes, childDecorations) = splitNodesAndDecorations childNodesWithDecorations
-- Inline decoration for text node.
-- TODO: See if making decoration a functor makes this case easier to write because in the second slot of the tuple we just want to map over the decoration structure.
pmTreeNodeFolder (Right (InlineDecoration (PMInlineDecoration decFrom decTo decAttrs (PMNode pmNode@(PM.TextNode _))))) _ =
  ([pmNode], [InlineDecoration $ PMInlineDecoration decFrom decTo decAttrs pmNode])
-- Widget decoration for text node.
pmTreeNodeFolder (Right (WidgetDecoration (PMWidgetDecoration decPos (PMNode pmNode@(PM.TextNode _))))) _ =
  ([pmNode], [WidgetDecoration $ PMWidgetDecoration decPos pmNode])
-- Widget decoration for wrapper inline node. Just return the children nodes and decorations (they will contain the decoration themselves)
pmTreeNodeFolder (Right (WidgetDecoration (PMWidgetDecoration _ (WrapperInlineNode)))) childNodesWithDecorations = splitNodesAndDecorations childNodesWithDecorations
-- Widget decoration for block node. Append the block decoration to the accumulated list of decorations.
-- TODO: See if making decoration a functor makes this case easier to write.
pmTreeNodeFolder (Right (WidgetDecoration (PMWidgetDecoration decPos (PMNode pmNode@(PM.BlockNode blockNode))))) childNodesWithDecorations =
  ([PM.BlockNode $ wrapChildrenToBlock blockNode childNodes], [blockDecoration] <> childDecorations)
  where
    blockDecoration = WidgetDecoration $ PMWidgetDecoration decPos pmNode
    (childNodes, childDecorations) = splitNodesAndDecorations childNodesWithDecorations
-- TODO: There are cases we didn't handle, like an inline decoration wrapping blocks.
-- These are failure cases and we should guard against them, ideally in the type system (with more accurate/specific types).
pmTreeNodeFolder _ _ = undefined

wrapChildrenToBlock :: PM.BlockNode -> [PM.Node] -> PM.BlockNode
wrapChildrenToBlock (PM.PMBlock blockType Nothing blockAttrs) children = PM.PMBlock blockType (Just children) blockAttrs
wrapChildrenToBlock (PM.PMBlock blockType (Just existingChildren) blockAttrs) newChildren = PM.PMBlock blockType (Just (existingChildren <> newChildren)) blockAttrs

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
  -- TODO: Use parameters for decoration class
  pure $ Right $ InlineDecoration $ wrapInInlineDecoration pmNode startIndex (startIndex + textLength) "bg-green-300"
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
  -- TODO: Use parameters for decoration class
  pure $ Right $ InlineDecoration $ wrapInInlineDecoration pmNode startIndex (startIndex + textLength) "bg-purple-100"
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

-- TODO: Use ProseMirror schema as a parameter
treeBlockNodeToPMBlockNode :: RichText.BlockNode -> PM.BlockNode
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Plain _)) = PM.PMBlock {PM.nodeType = "paragraph", PM.content = Nothing, PM.attrs = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Para _)) = PM.PMBlock {PM.nodeType = "paragraph", PM.content = Nothing, PM.attrs = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Header level _ _)) = PM.PMBlock {PM.nodeType = "heading", PM.content = Nothing, PM.attrs = Just $ KM.fromList [(K.fromText "level", Number (fromIntegral level))]}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.CodeBlock _ text)) = PM.PMBlock {PM.nodeType = "code_block", PM.content = Just $ [PM.TextNode $ PM.PMText {PM.text = text, PM.marks = Nothing}], PM.attrs = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.BulletList _)) = PM.PMBlock {PM.nodeType = "bullet_list", PM.content = Nothing, PM.attrs = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.OrderedList _ _)) = PM.PMBlock {PM.nodeType = "ordered_list", PM.content = Nothing, PM.attrs = Nothing}
treeBlockNodeToPMBlockNode (RichText.ListItem _) = PM.PMBlock {PM.nodeType = "list_item", PM.content = Nothing, PM.attrs = Nothing}
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
