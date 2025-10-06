{-# LANGUAGE OverloadedStrings #-}

module ProseMirror.Tree (PMTree, PMTreeNode (..), groupedInlinesPandocTreeToPMTree, pmDocFromPMTree, leafTextSpansPandocTreeNodeToPMNode, treeTextSpanNodeToPMTextNode, pmNodeFromInlineSpan, pmTreeFromPMDoc, pmTreeToGroupedInlinesTree) where

import Data.Aeson (Value (Number, String))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Graph (Tree (..))
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (listToMaybe)
import Data.Tree (foldTree)
import DocTree.Common as RichText (BlockNode (..), InlineSpan (..), LinkMark (..), Mark (..), NoteId (..), TextSpan (..))
import qualified DocTree.GroupedInlines as GroupedInlinesTree
import qualified DocTree.LeafTextSpans as LeafTextSpansTree
import qualified ProseMirror.Json as PM (BlockNode (..), Mark (..), Node (..), PMDoc (..), TextNode (..), wrapChildrenToBlock)
import Text.Pandoc.Builder as Pandoc
  ( Block (..),
    nullAttr,
  )

data PMTreeNode = PMNode PM.Node | WrapperInlineNode | WrapperBlockNode deriving (Show)

type PMTree = Tree PMTreeNode

pmDocFromPMTree :: PMTree -> Either String PM.PMDoc
pmDocFromPMTree pmTree = do
  pmDoc <- extractRootBlock $ foldTree pmTreeNodeFolder pmTree
  pure $ PM.PMDoc {PM.doc = pmDoc}
  where
    extractRootBlock :: [PM.Node] -> Either String PM.BlockNode
    extractRootBlock nodes = case listToMaybe nodes of
      Just node -> assertRootNodeIsBlock node
      Nothing -> Left "No root node found."

    assertRootNodeIsBlock :: PM.Node -> Either String PM.BlockNode
    assertRootNodeIsBlock (PM.BlockNode rootNode) = Right rootNode
    assertRootNodeIsBlock _ = Left "Root node is not a block node."

pmTreeNodeFolder :: PMTreeNode -> [[PM.Node]] -> [PM.Node]
pmTreeNodeFolder (PMNode pmNode@(PM.TextNode _)) _ = [pmNode]
pmTreeNodeFolder WrapperInlineNode childNodes = concat childNodes
pmTreeNodeFolder (WrapperBlockNode) childNodes = concat childNodes
pmTreeNodeFolder (PMNode (PM.BlockNode blockNode)) childNodes = [PM.BlockNode $ PM.wrapChildrenToBlock blockNode $ concat childNodes]

groupedInlinesPandocTreeToPMTree :: Tree GroupedInlinesTree.DocNode -> PMTree
groupedInlinesPandocTreeToPMTree (Node GroupedInlinesTree.Root childTrees) =
  Node (PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "doc", PM.content = Nothing, PM.attrs = Nothing}) (map groupedInlinesPandocTreeToPMTree childTrees)
groupedInlinesPandocTreeToPMTree (Node (GroupedInlinesTree.TreeNode (GroupedInlinesTree.BlockNode blockNode)) childTrees) =
  Node (treeBlockNodeToPMBlockNode blockNode) (map groupedInlinesPandocTreeToPMTree childTrees)
groupedInlinesPandocTreeToPMTree (Node (GroupedInlinesTree.TreeNode (GroupedInlinesTree.InlineNode (GroupedInlinesTree.InlineContent inlineSpans))) _) =
  Node WrapperInlineNode (map pmTreeFromInlineSpan inlineSpans)

leafTextSpansPandocTreeNodeToPMNode :: LeafTextSpansTree.DocNode -> PMTreeNode
leafTextSpansPandocTreeNodeToPMNode LeafTextSpansTree.Root = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "doc", PM.content = Nothing, PM.attrs = Nothing}
leafTextSpansPandocTreeNodeToPMNode (LeafTextSpansTree.TreeNode (LeafTextSpansTree.BlockNode node)) = treeBlockNodeToPMBlockNode node
leafTextSpansPandocTreeNodeToPMNode (LeafTextSpansTree.TreeNode (LeafTextSpansTree.InlineNode)) = WrapperInlineNode
leafTextSpansPandocTreeNodeToPMNode (LeafTextSpansTree.TreeNode (LeafTextSpansTree.InlineContent inlineSpan)) = pmNodeFromInlineSpan inlineSpan

pmTreeFromInlineSpan :: InlineSpan -> Tree PMTreeNode
pmTreeFromInlineSpan inlineSpan = Node (pmNodeFromInlineSpan inlineSpan) []

pmNodeFromInlineSpan :: InlineSpan -> PMTreeNode
pmNodeFromInlineSpan (InlineText textSpan) = PMNode (PM.TextNode (treeTextSpanNodeToPMTextNode textSpan))
pmNodeFromInlineSpan (NoteRef (NoteId noteId)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "note_ref", PM.content = Nothing, PM.attrs = Just $ KM.fromList [(K.fromText "id", Data.Aeson.String noteId)]}

-- TODO: Use ProseMirror schema as a parameter
treeBlockNodeToPMBlockNode :: RichText.BlockNode -> PMTreeNode
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Plain _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "paragraph", PM.content = Nothing, PM.attrs = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Para _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "paragraph", PM.content = Nothing, PM.attrs = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Header level _ _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "heading", PM.content = Nothing, PM.attrs = Just $ KM.fromList [(K.fromText "level", Data.Aeson.Number (fromIntegral level))]}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.CodeBlock _ _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "code_block", PM.content = Nothing, PM.attrs = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.BulletList _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "bullet_list", PM.content = Nothing, PM.attrs = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.OrderedList _ _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "ordered_list", PM.content = Nothing, PM.attrs = Nothing}
treeBlockNodeToPMBlockNode (RichText.ListItem _) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "list_item", PM.content = Nothing, PM.attrs = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.BlockQuote _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "blockquote", PM.content = Nothing, PM.attrs = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Div _ _)) = WrapperBlockNode
treeBlockNodeToPMBlockNode (RichText.NoteContent (NoteId noteId) _) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "note_content", PM.content = Nothing, PM.attrs = Just $ KM.fromList [(K.fromText "id", Data.Aeson.String noteId)]}
-- TODO: Incrementally handle more blocks
treeBlockNodeToPMBlockNode _ = undefined

-- TODO: Use ProseMirror schema as a parameter
treeTextSpanNodeToPMTextNode :: RichText.TextSpan -> PM.TextNode
treeTextSpanNodeToPMTextNode textSpan = PM.PMText {PM.text = value textSpan, PM.marks = (fmap . fmap) toPMMark (nonEmpty $ marks textSpan)}
  where
    toPMMark :: RichText.Mark -> PM.Mark
    toPMMark RichText.EmphMark = PM.PMMark {PM.markType = "em", PM.markAttrs = Nothing}
    toPMMark RichText.StrongMark = PM.PMMark {PM.markType = "strong", PM.markAttrs = Nothing}
    toPMMark (RichText.LinkMark (RichText.Link _ (linkUrl, linkTitle))) = PM.PMMark {PM.markType = "link", PM.markAttrs = Just $ KM.fromList [(K.fromText "href", Data.Aeson.String linkUrl), (K.fromText "title", Data.Aeson.String linkTitle)]}
    toPMMark RichText.CodeMark = PM.PMMark {PM.markType = "code", PM.markAttrs = Nothing}

pmTreeFromPMDoc :: PM.PMDoc -> PMTree
pmTreeFromPMDoc = undefined

pmTreeToGroupedInlinesTree :: PMTree -> Tree GroupedInlinesTree.DocNode
pmTreeToGroupedInlinesTree = undefined

-- TODO: Use ProseMirror schema as a parameter
pmNodeToTreeNode :: PMTreeNode -> RichText.BlockNode
pmNodeToTreeNode (WrapperBlockNode) = RichText.PandocBlock $ Pandoc.Div nullAttr []
pmNodeToTreeNode (WrapperInlineNode) = undefined
pmNodeToTreeNode (PMNode (PM.BlockNode pmBlockNode)) = case PM.nodeType pmBlockNode of
  -- Node hierarchy is represented in the tree structure, so we always use an empty list of children in the Pandoc blocks
  "paragraph" -> RichText.PandocBlock $ Pandoc.Para []
  -- TODO: Incrementally handle more blocks
  _ -> undefined
pmNodeToTreeNode (PMNode (PM.TextNode pmTextNode)) = undefined
