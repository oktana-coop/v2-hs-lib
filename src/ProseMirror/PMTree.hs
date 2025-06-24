{-# LANGUAGE OverloadedStrings #-}

module ProseMirror.PMTree (PMTree, PMTreeNode (..), groupedInlinesPandocTreeToPMTree, pmDocFromPMTree, leafTextSpansPandocTreeNodeToPMNode, treeTextSpanNodeToPMTextNode) where

import Data.Aeson (Value (Number, String))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Graph (Tree (..))
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (listToMaybe)
import Data.Tree (foldTree)
import DocTree.Common as RichText (BlockNode (..), LinkMark (..), Mark (..), TextSpan (..))
import qualified DocTree.GroupedInlines as GroupedInlinesTree
import qualified DocTree.LeafTextSpans as LeafTextSpansTree
import qualified ProseMirror.PMJson as PM (BlockNode (..), Mark (..), Node (..), PMDoc (..), TextNode (..), wrapChildrenToBlock)
import Text.Pandoc.Definition as Pandoc (Block (..))

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
groupedInlinesPandocTreeToPMTree (Node (GroupedInlinesTree.TreeNode (GroupedInlinesTree.InlineNode (GroupedInlinesTree.InlineContent textSpans))) _) =
  Node WrapperInlineNode (map pmTreeFromTextSpan textSpans)
  where
    pmTreeFromTextSpan :: TextSpan -> Tree PMTreeNode
    pmTreeFromTextSpan textSpan = Node (PMNode (PM.TextNode (treeTextSpanNodeToPMTextNode textSpan))) []

leafTextSpansPandocTreeNodeToPMNode :: LeafTextSpansTree.DocNode -> PMTreeNode
leafTextSpansPandocTreeNodeToPMNode LeafTextSpansTree.Root = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "doc", PM.content = Nothing, PM.attrs = Nothing}
leafTextSpansPandocTreeNodeToPMNode (LeafTextSpansTree.TreeNode (LeafTextSpansTree.BlockNode node)) = treeBlockNodeToPMBlockNode node
leafTextSpansPandocTreeNodeToPMNode (LeafTextSpansTree.TreeNode (LeafTextSpansTree.InlineNode)) = WrapperInlineNode
leafTextSpansPandocTreeNodeToPMNode (LeafTextSpansTree.TreeNode (LeafTextSpansTree.InlineContent textSpan)) = PMNode $ PM.TextNode $ treeTextSpanNodeToPMTextNode textSpan

-- TODO: Use ProseMirror schema as a parameter
treeBlockNodeToPMBlockNode :: RichText.BlockNode -> PMTreeNode
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Plain _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "paragraph", PM.content = Nothing, PM.attrs = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Para _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "paragraph", PM.content = Nothing, PM.attrs = Nothing}
treeBlockNodeToPMBlockNode (RichText.PandocBlock (Pandoc.Header level _ _)) = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "heading", PM.content = Nothing, PM.attrs = Just $ KM.fromList [(K.fromText "level", Data.Aeson.Number (fromIntegral level))]}
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
    toPMMark RichText.EmphMark = PM.PMMark {PM.markType = "em", PM.markAttrs = Nothing}
    toPMMark RichText.StrongMark = PM.PMMark {PM.markType = "strong", PM.markAttrs = Nothing}
    toPMMark (RichText.LinkMark (RichText.Link _ (linkUrl, linkTitle))) = PM.PMMark {PM.markType = "link", PM.markAttrs = Just $ KM.fromList [(K.fromText "href", Data.Aeson.String linkUrl), (K.fromText "title", Data.Aeson.String linkTitle)]}
    toPMMark RichText.CodeMark = PM.PMMark {PM.markType = "code", PM.markAttrs = Nothing}