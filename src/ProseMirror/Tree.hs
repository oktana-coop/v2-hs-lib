{-# LANGUAGE OverloadedStrings #-}

module ProseMirror.Tree (PMTree, PMTreeNode (..), groupedInlinesPandocTreeToPMTree, pmDocFromPMTree, leafTextSpansPandocTreeNodeToPMNode, treeTextSpanNodeToPMTextNode, pmNodeFromInlineSpan, pmTreeFromPMDoc, pmTreeToGroupedInlinesTree) where

import Data.Graph (Tree (..))
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (listToMaybe)
import Data.Tree (foldTree)
import DocTree.Common as RichText (BlockNode (..), InlineSpan (..), LinkMark (..), Mark (..), NoteId (..), TextSpan (..))
import qualified DocTree.GroupedInlines as GroupedInlinesTree
import qualified DocTree.LeafTextSpans as LeafTextSpansTree
import qualified ProseMirror.Model as PM (Block (..), BlockNode (..), HeadingLevel (..), Link (..), Mark (..), Node (..), NoteId (..), PMDoc (..), TextNode (..), assertRootNodeIsDoc, wrapChildrenToBlock)
import Text.Pandoc.Builder as Pandoc
  ( Block (..),
    nullAttr,
  )

data PMTreeNode = PMNode PM.Node | WrapperInlineNode | WrapperBlockNode deriving (Show)

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
pmTreeNodeFolder (PMNode (PM.BlockNode blockNode)) childNodes = [PM.BlockNode $ PM.wrapChildrenToBlock blockNode $ concat childNodes]

groupedInlinesPandocTreeToPMTree :: Tree GroupedInlinesTree.DocNode -> PMTree
groupedInlinesPandocTreeToPMTree (Node GroupedInlinesTree.Root childTrees) =
  Node (PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.Doc, PM.fragment = Nothing}) (map groupedInlinesPandocTreeToPMTree childTrees)
groupedInlinesPandocTreeToPMTree (Node (GroupedInlinesTree.TreeNode (GroupedInlinesTree.BlockNode blockNode)) childTrees) =
  Node (treeBlockNodeToPMBlockNode blockNode) (map groupedInlinesPandocTreeToPMTree childTrees)
groupedInlinesPandocTreeToPMTree (Node (GroupedInlinesTree.TreeNode (GroupedInlinesTree.InlineNode (GroupedInlinesTree.InlineContent inlineSpans))) _) =
  Node WrapperInlineNode (map pmTreeFromInlineSpan inlineSpans)

leafTextSpansPandocTreeNodeToPMNode :: LeafTextSpansTree.DocNode -> PMTreeNode
leafTextSpansPandocTreeNodeToPMNode LeafTextSpansTree.Root = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.Doc, PM.fragment = Nothing}
leafTextSpansPandocTreeNodeToPMNode (LeafTextSpansTree.TreeNode (LeafTextSpansTree.BlockNode node)) = treeBlockNodeToPMBlockNode node
leafTextSpansPandocTreeNodeToPMNode (LeafTextSpansTree.TreeNode (LeafTextSpansTree.InlineNode)) = WrapperInlineNode
leafTextSpansPandocTreeNodeToPMNode (LeafTextSpansTree.TreeNode (LeafTextSpansTree.InlineContent inlineSpan)) = pmNodeFromInlineSpan inlineSpan

pmTreeFromInlineSpan :: InlineSpan -> Tree PMTreeNode
pmTreeFromInlineSpan inlineSpan = Node (pmNodeFromInlineSpan inlineSpan) []

pmNodeFromInlineSpan :: InlineSpan -> PMTreeNode
pmNodeFromInlineSpan (InlineText textSpan) = PMNode (PM.TextNode (treeTextSpanNodeToPMTextNode textSpan))
pmNodeFromInlineSpan (NoteRef (NoteId noteId)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.NoteRef $ PM.NoteId noteId, PM.fragment = Nothing}

-- TODO: Use ProseMirror schema as a parameter
treeBlockNodeToPMBlockNode :: RichText.BlockNode -> PMTreeNode
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Plain _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.Paragraph, PM.fragment = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Para _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.Paragraph, PM.fragment = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Header level _ _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.Heading $ PM.HeadingLevel level, PM.fragment = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.CodeBlock _ _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.CodeBlock, PM.fragment = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.BulletList _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.BulletList, PM.fragment = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.OrderedList _ _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.OrderedList, PM.fragment = Nothing}
treeBlockNodeToPMBlockNode (RichText.ListItem _) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.ListItem, PM.fragment = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.BlockQuote _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.BlockQuote, PM.fragment = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Div _ _)) = WrapperBlockNode
treeBlockNodeToPMBlockNode (RichText.NoteContent (NoteId noteId) _) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.block = PM.NoteContent $ PM.NoteId noteId, PM.fragment = Nothing}
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
pmTreeFromPMDoc = undefined

pmTreeToGroupedInlinesTree :: PMTree -> Tree GroupedInlinesTree.DocNode
pmTreeToGroupedInlinesTree = undefined

-- TODO: Use ProseMirror schema as a parameter
pmNodeToTreeNode :: PMTreeNode -> RichText.BlockNode
pmNodeToTreeNode (WrapperBlockNode) = RichText.PandocBlock $ Pandoc.Div nullAttr []
pmNodeToTreeNode (WrapperInlineNode) = undefined
pmNodeToTreeNode (PMNode (PM.BlockNode (PM.PMBlock (PM.Paragraph) _))) = RichText.PandocBlock $ Pandoc.Para []
pmNodeToTreeNode (PMNode (PM.TextNode pmTextNode)) = undefined
pmNodeToTreeNode _ = undefined
