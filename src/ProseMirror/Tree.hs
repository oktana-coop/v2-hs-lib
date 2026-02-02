{-# LANGUAGE OverloadedStrings #-}

module ProseMirror.Tree (PMTree, PMTreeNode (..), groupedInlinesPandocTreeToPMTree, pmDocFromPMTree, leafTextSpansPandocTreeNodeToPMNode, treeTextSpanNodeToPMTextNode, pmNodeFromInlineSpan, pmTreeFromPMDoc, pmTreeToGroupedInlinesTree) where

import Data.List.NonEmpty (nonEmpty, toList)
import Data.Maybe (listToMaybe, maybeToList)
import qualified Data.Text as T
import Data.Tree (Tree (..), foldTree, unfoldTree)
import DocTree.Common as RichText (BlockNode (..), InlineSpan (..), LinkMark (..), Mark (..), NoteId (..), TextSpan (..))
import qualified DocTree.GroupedInlines as GroupedInlinesTree
import qualified DocTree.LeafTextSpans as LeafTextSpansTree
import GHC.Base (NonEmpty)
import qualified ProseMirror.Model as PM (Block (..), BlockNode (..), HeadingLevel (..), Link (..), Mark (..), Node (..), NoteId (..), PMDoc (..), TextNode (..), assertRootNodeIsDoc, wrapChildrenToBlock)
import Text.Pandoc.Builder as Pandoc
  ( Block (..),
    ListNumberDelim (DefaultDelim),
    ListNumberStyle (DefaultStyle),
    nullAttr,
  )

data PMTreeNode
  = PMNode PM.Node
  | WrapperInlineNode
  | WrapperBlockNode
  | -- Nodes that cannot be represented in ProseMirror.
    UnrepresentableNode
  deriving (Show)

type PMTree = Tree PMTreeNode

pmDocFromPMTree :: PMTree -> Either String PM.PMDoc
pmDocFromPMTree pmTree = extractRootBlock $ foldTree pmTreeNodeFolder pmTree
  where
    extractRootBlock :: [PM.Node] -> Either String PM.PMDoc
    extractRootBlock nodes = case listToMaybe nodes of
      Just node -> PM.assertRootNodeIsDoc node
      Nothing -> Left "No root node found."

pmTreeNodeFolder :: PMTreeNode -> [[PM.Node]] -> [PM.Node]
pmTreeNodeFolder (PMNode pmNode@(PM.TextNode _)) _ = [pmNode]
pmTreeNodeFolder WrapperInlineNode childNodes = concat childNodes
pmTreeNodeFolder (WrapperBlockNode) childNodes = concat childNodes
-- Ignore nodes that cannot be represented in ProseMirror
pmTreeNodeFolder (UnrepresentableNode) _ = []
pmTreeNodeFolder (PMNode (PM.BlockNode blockNode)) childNodes = [PM.BlockNode $ PM.wrapChildrenToBlock blockNode $ concat childNodes]

groupedInlinesPandocTreeToPMTree :: Tree GroupedInlinesTree.DocNode -> PMTree
groupedInlinesPandocTreeToPMTree (Node GroupedInlinesTree.Root childTrees) =
  Node (PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.Doc, PM.content = Nothing}) (map groupedInlinesPandocTreeToPMTree childTrees)
groupedInlinesPandocTreeToPMTree (Node (GroupedInlinesTree.TreeNode (GroupedInlinesTree.BlockNode blockNode)) childTrees) =
  Node (treeBlockNodeToPMBlockNode blockNode) (map groupedInlinesPandocTreeToPMTree childTrees)
groupedInlinesPandocTreeToPMTree (Node (GroupedInlinesTree.TreeNode (GroupedInlinesTree.InlineNode (GroupedInlinesTree.InlineContent inlineSpans))) _) =
  Node WrapperInlineNode (map pmTreeFromInlineSpan inlineSpans)

leafTextSpansPandocTreeNodeToPMNode :: LeafTextSpansTree.DocNode -> PMTreeNode
leafTextSpansPandocTreeNodeToPMNode LeafTextSpansTree.Root = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.Doc, PM.content = Nothing}
leafTextSpansPandocTreeNodeToPMNode (LeafTextSpansTree.TreeNode (LeafTextSpansTree.BlockNode node)) = treeBlockNodeToPMBlockNode node
leafTextSpansPandocTreeNodeToPMNode (LeafTextSpansTree.TreeNode (LeafTextSpansTree.InlineNode)) = WrapperInlineNode
leafTextSpansPandocTreeNodeToPMNode (LeafTextSpansTree.TreeNode (LeafTextSpansTree.InlineContent inlineSpan)) = pmNodeFromInlineSpan inlineSpan

pmTreeFromInlineSpan :: InlineSpan -> Tree PMTreeNode
pmTreeFromInlineSpan inlineSpan = Node (pmNodeFromInlineSpan inlineSpan) []

pmNodeFromInlineSpan :: InlineSpan -> PMTreeNode
pmNodeFromInlineSpan (InlineText textSpan) = PMNode (PM.TextNode (treeTextSpanNodeToPMTextNode textSpan))
pmNodeFromInlineSpan (NoteRef (NoteId noteId)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.NoteRef $ PM.NoteId noteId, PM.content = Nothing}

-- TODO: Use ProseMirror schema as a parameter
treeBlockNodeToPMBlockNode :: RichText.BlockNode -> PMTreeNode
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Plain _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.Paragraph, PM.content = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Para _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.Paragraph, PM.content = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Header level _ _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.Heading $ PM.HeadingLevel level, PM.content = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.CodeBlock _ _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.CodeBlock, PM.content = Nothing}
-- TODO: Potentially handle raw blocks with format "prosemirror" or "html"".
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.RawBlock _ _)) = UnrepresentableNode
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.BulletList _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.BulletList, PM.content = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.OrderedList _ _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.OrderedList, PM.content = Nothing}
treeBlockNodeToPMBlockNode (RichText.ListItem _) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.ListItem, PM.content = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.BlockQuote _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.BlockQuote, PM.content = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Div _ _)) = WrapperBlockNode
treeBlockNodeToPMBlockNode (RichText.NoteContent (NoteId noteId) _) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.NoteContent $ PM.NoteId noteId, PM.content = Nothing}
-- TODO: Incrementally handle more blocks
treeBlockNodeToPMBlockNode _ = undefined

-- TODO: Use ProseMirror schema as a parameter
treeTextSpanNodeToPMTextNode :: RichText.TextSpan -> PM.TextNode
treeTextSpanNodeToPMTextNode textSpan = PM.PMText {PM.text = value textSpan, PM.marks = (fmap . fmap) toPMMark (nonEmpty $ marks textSpan)}
  where
    toPMMark :: RichText.Mark -> PM.Mark
    toPMMark RichText.EmphMark = PM.Emphasis
    toPMMark RichText.StrongMark = PM.Strong
    toPMMark (RichText.LinkMark (RichText.Link _ (linkUrl, linkTitle))) = PM.LinkMark $ PM.Link {PM.url = linkUrl, PM.title = linkTitle}
    toPMMark RichText.CodeMark = PM.Code

pmTreeFromPMDoc :: PM.PMDoc -> PMTree
pmTreeFromPMDoc (PM.PMDoc rootNode) = unfoldTree pmTreeFromPMDocUnfolder rootNode

pmTreeFromPMDocUnfolder :: PM.Node -> (PMTreeNode, [PM.Node])
pmTreeFromPMDocUnfolder (PM.BlockNode (PM.PMBlock bl children)) =
  (PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = bl, PM.content = Nothing}, concat $ maybeToList children)
pmTreeFromPMDocUnfolder textNode@(PM.TextNode _) = (PMNode textNode, [])

pmTreeToGroupedInlinesTree :: PMTree -> Tree GroupedInlinesTree.DocNode
pmTreeToGroupedInlinesTree = foldTree pmNodeToGroupedInlinesNodeFolder

pmNodeToGroupedInlinesNodeFolder :: PMTreeNode -> [Tree GroupedInlinesTree.DocNode] -> Tree GroupedInlinesTree.DocNode
pmNodeToGroupedInlinesNodeFolder (PMNode (PM.BlockNode (PM.PMBlock (PM.Doc) _))) childTrees =
  Node GroupedInlinesTree.Root childTrees
pmNodeToGroupedInlinesNodeFolder (PMNode (PM.BlockNode (PM.PMBlock (PM.Paragraph) _))) childTrees =
  Node (GroupedInlinesTree.TreeNode $ GroupedInlinesTree.BlockNode $ RichText.PandocBlock $ Pandoc.Para []) (concatAdjacentInlineNodes childTrees)
pmNodeToGroupedInlinesNodeFolder (PMNode (PM.BlockNode (PM.PMBlock (PM.Heading (PM.HeadingLevel level)) _))) childTrees =
  Node (GroupedInlinesTree.TreeNode $ GroupedInlinesTree.BlockNode $ RichText.PandocBlock $ Pandoc.Header level nullAttr []) (concatAdjacentInlineNodes childTrees)
pmNodeToGroupedInlinesNodeFolder (PMNode (PM.BlockNode (PM.PMBlock (PM.CodeBlock) _))) childTrees =
  Node (GroupedInlinesTree.TreeNode $ GroupedInlinesTree.BlockNode $ RichText.PandocBlock $ Pandoc.CodeBlock nullAttr T.empty) (concatAdjacentInlineNodes childTrees)
pmNodeToGroupedInlinesNodeFolder (PMNode (PM.BlockNode (PM.PMBlock (PM.BulletList) _))) childTrees =
  Node (GroupedInlinesTree.TreeNode $ GroupedInlinesTree.BlockNode $ RichText.PandocBlock $ Pandoc.BulletList []) childTrees
pmNodeToGroupedInlinesNodeFolder (PMNode (PM.BlockNode (PM.PMBlock (PM.OrderedList) _))) childTrees =
  Node (GroupedInlinesTree.TreeNode $ GroupedInlinesTree.BlockNode $ RichText.PandocBlock $ Pandoc.OrderedList (1, DefaultStyle, DefaultDelim) []) childTrees
pmNodeToGroupedInlinesNodeFolder (PMNode (PM.BlockNode (PM.PMBlock (PM.ListItem) _))) childTrees =
  Node (GroupedInlinesTree.TreeNode $ GroupedInlinesTree.BlockNode $ RichText.ListItem []) (compactListIfPossible $ concatAdjacentInlineNodes childTrees)
pmNodeToGroupedInlinesNodeFolder (PMNode (PM.BlockNode (PM.PMBlock (PM.BlockQuote) _))) childTrees =
  Node (GroupedInlinesTree.TreeNode $ GroupedInlinesTree.BlockNode $ RichText.PandocBlock $ Pandoc.BlockQuote []) (concatAdjacentInlineNodes childTrees)
pmNodeToGroupedInlinesNodeFolder (PMNode (PM.BlockNode (PM.PMBlock (PM.NoteContent (PM.NoteId noteId)) _))) childTrees =
  Node (GroupedInlinesTree.TreeNode $ GroupedInlinesTree.BlockNode $ RichText.NoteContent (RichText.NoteId noteId) []) (concatAdjacentInlineNodes childTrees)
pmNodeToGroupedInlinesNodeFolder WrapperBlockNode childTrees =
  Node (GroupedInlinesTree.TreeNode $ GroupedInlinesTree.BlockNode $ RichText.PandocBlock $ Pandoc.Div nullAttr []) (concatAdjacentInlineNodes childTrees)
-- In the case of inline nodes we produce an `InlineNode` with a list of a single inline span when processing the node.
-- When processing container blocks we are concatenating adjacent inline nodes, so we end up with a single `InlineNode` containing a list of inline spans.
pmNodeToGroupedInlinesNodeFolder (PMNode (PM.BlockNode (PM.PMBlock (PM.NoteRef (PM.NoteId noteId)) _))) _ =
  Node (GroupedInlinesTree.TreeNode $ GroupedInlinesTree.InlineNode $ GroupedInlinesTree.InlineContent [RichText.NoteRef (RichText.NoteId noteId)]) []
pmNodeToGroupedInlinesNodeFolder (PMNode (PM.TextNode textNode)) _ =
  Node (GroupedInlinesTree.TreeNode $ GroupedInlinesTree.InlineNode $ GroupedInlinesTree.InlineContent [RichText.InlineText $ pmTextNodeToTreeTextSpan textNode]) []
pmNodeToGroupedInlinesNodeFolder WrapperInlineNode childTrees =
  Node (GroupedInlinesTree.TreeNode $ GroupedInlinesTree.InlineNode $ GroupedInlinesTree.InlineContent []) (concatAdjacentInlineNodes childTrees)
-- Technically, we should never get unrepresentable nodes as input. TODO: Handle this case better, maybe with an error.
pmNodeToGroupedInlinesNodeFolder UnrepresentableNode childTrees =
  Node (GroupedInlinesTree.TreeNode $ GroupedInlinesTree.BlockNode $ RichText.PandocBlock $ Pandoc.RawBlock "prosemirror" T.empty) (concatAdjacentInlineNodes childTrees)

pmTextNodeToTreeTextSpan :: PM.TextNode -> RichText.TextSpan
pmTextNodeToTreeTextSpan (PM.PMText t pmMarks) = RichText.TextSpan t (map toTreeMark $ maybeNonEmptyToList pmMarks)
  where
    maybeNonEmptyToList :: Maybe (NonEmpty a) -> [a]
    maybeNonEmptyToList = maybe [] toList

    toTreeMark :: PM.Mark -> RichText.Mark
    toTreeMark PM.Strong = RichText.StrongMark
    toTreeMark PM.Emphasis = RichText.EmphMark
    toTreeMark (PM.LinkMark (PM.Link url title)) = RichText.LinkMark $ RichText.Link nullAttr (url, title)
    toTreeMark PM.Code = RichText.CodeMark

concatAdjacentInlineNodes :: [Tree GroupedInlinesTree.DocNode] -> [Tree GroupedInlinesTree.DocNode]
concatAdjacentInlineNodes = foldr mergeOrAppendAdjacent []
  where
    mergeOrAppendAdjacent :: Tree GroupedInlinesTree.DocNode -> [Tree GroupedInlinesTree.DocNode] -> [Tree GroupedInlinesTree.DocNode]
    mergeOrAppendAdjacent x [] = [x]
    -- Merge adjacent inline nodes
    mergeOrAppendAdjacent (Node (GroupedInlinesTree.TreeNode (GroupedInlinesTree.InlineNode inlineNode1)) childTrees1) ((Node (GroupedInlinesTree.TreeNode (GroupedInlinesTree.InlineNode inlineNode2)) childTrees2) : rest) =
      Node (GroupedInlinesTree.TreeNode $ GroupedInlinesTree.InlineNode $ (inlineNode1 <> inlineNode2)) (childTrees1 <> childTrees2) : rest
    -- Do not merge in any other case
    mergeOrAppendAdjacent x rest = x : rest

-- Making a list compact requires mapping `Para` blocks to `Plain` ones.
-- We want to do this when a list has a single paragraph child, optionally followed by one or more list siblings.
compactListIfPossible :: [Tree GroupedInlinesTree.DocNode] -> [Tree GroupedInlinesTree.DocNode]
compactListIfPossible childTrees = case childTrees of
  [] -> []
  -- This is the case where the list has a single tree (single-child case).
  (Node root children : rest) | shouldConvertParaToPlain childTrees -> Node (paraToPlain root) children : rest
  _ -> childTrees
  where
    shouldConvertParaToPlain :: [Tree GroupedInlinesTree.DocNode] -> Bool
    shouldConvertParaToPlain trees = case trees of
      [] -> False
      [t] -> isParaTree t
      (t : rest) -> isParaTree t && all isListTree rest

    isParaTree :: Tree GroupedInlinesTree.DocNode -> Bool
    isParaTree (Node root _) = isParaNode root

    isListTree :: Tree GroupedInlinesTree.DocNode -> Bool
    isListTree (Node root _) = isListNode root

    isParaNode :: GroupedInlinesTree.DocNode -> Bool
    isParaNode (GroupedInlinesTree.TreeNode (GroupedInlinesTree.BlockNode (RichText.PandocBlock (Pandoc.Para _)))) = True
    isParaNode _ = False

    isListNode :: GroupedInlinesTree.DocNode -> Bool
    isListNode (GroupedInlinesTree.TreeNode (GroupedInlinesTree.BlockNode (RichText.PandocBlock (Pandoc.BulletList _)))) = True
    isListNode (GroupedInlinesTree.TreeNode (GroupedInlinesTree.BlockNode (RichText.PandocBlock (Pandoc.OrderedList _ _)))) = True
    isListNode (GroupedInlinesTree.TreeNode (GroupedInlinesTree.BlockNode (RichText.PandocBlock (Pandoc.DefinitionList _)))) = True
    isListNode _ = False

    paraToPlain :: GroupedInlinesTree.DocNode -> GroupedInlinesTree.DocNode
    paraToPlain (GroupedInlinesTree.TreeNode (GroupedInlinesTree.BlockNode (RichText.PandocBlock (Pandoc.Para children)))) =
      (GroupedInlinesTree.TreeNode $ GroupedInlinesTree.BlockNode $ RichText.PandocBlock $ Pandoc.Plain children)
    paraToPlain node = node
