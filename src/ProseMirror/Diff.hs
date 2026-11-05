{-# LANGUAGE OverloadedStrings #-}

module ProseMirror.Diff (toDecoratedPMDoc, DecoratedPMDoc (..), PositionedDiffNode, addPositionsToDiffTree) where

import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.Tree (Tree (..), foldTree)
import qualified DocTree.LeafTextSpans as PandocTree
import NodeDiff.Common (DiffOpType (..))
import NodeDiff.LeafTextSpans (DocNodeDiff, toDiffOpType, toDocNode)
import ProseMirror.Decoration (Decoration (..), DecorationAttrs (..), InlineDecoration (..), NodeDecoration (..), WidgetDecoration (..), undecorate)
import ProseMirror.Metadata (addDocFrontMatter)
import ProseMirror.Model (PMPosition, Positioned (..), addNodePositionsRenderedBy)
import qualified ProseMirror.Model as PM (FrontMatter, InlineNode (..), Node (..), isLeafBlockNode, wrapChildrenToBlock)
import ProseMirror.PandocTreeShape.FigureContent.LeafTextSpans (unwrapFigureContentParaOrPlain)
import ProseMirror.PandocTreeShape.Metadata.LeafTextSpans (withoutMetadataSubtree)
import ProseMirror.Tree (PMTreeNode (..), leafTextSpansPandocTreeNodeToPMNode)

-- Alias to the function exposed from the PMTree module
pandocTreeNodeToPMNode :: PandocTree.DocNode -> PMTreeNode
pandocTreeNodeToPMNode = leafTextSpansPandocTreeNodeToPMNode

type PositionedDiffNode = Positioned DocNodeDiff

type DecoratedPMTree = Tree (Either PMTreeNode (Decoration PMTreeNode))

data DecoratedPMDoc = DecoratedPMDoc {doc :: PM.Node, decorations :: [Decoration PM.Node]} deriving (Show, Eq)

-- The doc's attribute holds the given front matter.
toDecoratedPMDoc :: Maybe PM.FrontMatter -> Tree DocNodeDiff -> DecoratedPMDoc
toDecoratedPMDoc frontMatter = addFrontMatter . pmDocFromPMTree . fmap decorate . addPositionsToDiffTree
  where
    addFrontMatter :: DecoratedPMDoc -> DecoratedPMDoc
    addFrontMatter decoratedPMDoc = decoratedPMDoc {doc = addDocFrontMatter frontMatter (doc decoratedPMDoc)}

addPositionsToDiffTree :: Tree DocNodeDiff -> Tree PositionedDiffNode
addPositionsToDiffTree =
  addNodePositionsRenderedBy unpackAndRenderPMNode
    . unwrapFigureContentParaOrPlain toDocNode
    . withoutMetadataSubtree toDocNode
  where
    -- The ProseMirror node a diff node renders to, if any: deleted content is not part of the document and
    -- wrapper nodes have no ProseMirror counterpart, so neither takes positions.
    unpackAndRenderPMNode :: DocNodeDiff -> Maybe PM.Node
    unpackAndRenderPMNode nodeWithDiff
      | toDiffOpType nodeWithDiff == DeleteType = Nothing
      | otherwise = case pandocTreeNodeToPMNode (toDocNode nodeWithDiff) of
          PMNode node -> Just node
          _ -> Nothing

diffInsertClass :: T.Text
diffInsertClass = "diff-insert"

diffModifyClass :: T.Text
diffModifyClass = "diff-modify"

decorate :: PositionedDiffNode -> Either PMTreeNode (Decoration PMTreeNode)
decorate positionedNodeWithDiff = case toDiffOpType nodeWithDiff of
  CopyType -> Left node
  InsertType -> case node of
    -- Inserted leaf blocks (e.g. horizontal rule) have no inline children to carry an insert
    -- decoration, so we wrap the block itself in a node decoration instead.
    PMNode pm@(PM.BlockNode _) | PM.isLeafBlockNode pm -> Right $ NodeDecoration $ wrapInNodeDecoration node start end diffInsertClass
    PMNode (PM.InlineNode inlineNode) -> Right $ decorateInlineNode inlineNode diffInsertClass
    -- Other nodes (blocks, wrappers) are left undecorated: their changed children carry the decoration.
    _ -> Left node
  -- Deleted content is not part of the document; it is shown as a widget at the position it was removed from.
  DeleteType -> Right $ WidgetDecoration $ wrapInWidgetDecoration node start
  MarksUpdateType -> case node of
    PMNode (PM.InlineNode inlineNode) -> Right $ decorateInlineNode inlineNode diffModifyClass
    -- A marks update is on a text span, which renders to an inline node.
    _ -> Left node
  HeadingLevelUpdateType -> Right $ NodeDecoration $ wrapInNodeDecoration node start end diffModifyClass
  -- Left out before the positions: the doc's attribute holds the metadata.
  MetaValueUpdateType -> Left node
  where
    nodeWithDiff = value positionedNodeWithDiff
    node = pandocTreeNodeToPMNode $ toDocNode nodeWithDiff
    start = startPos positionedNodeWithDiff
    end = endPos positionedNodeWithDiff

    decorateInlineNode :: PM.InlineNode -> T.Text -> Decoration PMTreeNode
    decorateInlineNode inlineNode cssClassName = case inlineNode of
      PM.InlineText _ -> InlineDecoration $ wrapInInlineDecoration pmTreeNode start end cssClassName
      _ -> NodeDecoration $ wrapInNodeDecoration pmTreeNode start end cssClassName
      where
        pmTreeNode = PMNode $ PM.InlineNode inlineNode

wrapInInlineDecoration :: PMTreeNode -> PMPosition -> PMPosition -> T.Text -> InlineDecoration PMTreeNode
wrapInInlineDecoration pmNode fromIndex toIndex className =
  PMInlineDecoration
    { inlineDecFrom = fromIndex,
      inlineDecTo = toIndex,
      inlineDecAttrs =
        DecorationAttrs
          { nodeName = Nothing,
            cssClass = Just className,
            style = Nothing
          },
      inlineDecContent = pmNode
    }

wrapInNodeDecoration :: PMTreeNode -> PMPosition -> PMPosition -> T.Text -> NodeDecoration PMTreeNode
wrapInNodeDecoration pmNode fromIndex toIndex cssClassName =
  PMNodeDecoration
    { nodeDecFrom = fromIndex,
      nodeDecTo = toIndex,
      nodeDecAttrs =
        DecorationAttrs
          { nodeName = Nothing,
            cssClass = Just cssClassName,
            style = Nothing
          },
      nodeDecContent = pmNode
    }

wrapInWidgetDecoration :: PMTreeNode -> PMPosition -> WidgetDecoration PMTreeNode
wrapInWidgetDecoration pmNode position =
  PMWidgetDecoration
    { pos = position,
      widgetDecContent = pmNode
    }

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
