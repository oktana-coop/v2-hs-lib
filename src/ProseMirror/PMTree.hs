{-# LANGUAGE OverloadedStrings #-}

module ProseMirror.PMTree (PMTree, PMTreeNode (..), groupedInlinesPandocTreeToPMTree, pmDocFromPMTree, leafTextSpansTreeNodeToPMNode, treeTextSpanNodeToPMTextNode) where

import Data.Aeson (Value (Number, String))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List.NonEmpty (nonEmpty)
import Data.Tree (Tree)
import DocTree.Common as RichText (BlockNode (..), LinkMark (..), Mark (..), TextSpan (..))
import qualified DocTree.GroupedInlines as GroupedInlinesTree
import qualified DocTree.LeafTextSpans as LeafTextSpansTree
import qualified ProseMirror.PMJson as PM (BlockNode (..), Mark (..), Node (..), PMDoc (..), TextNode (..))
import Text.Pandoc.Definition as Pandoc (Block (..))

data PMTreeNode = PMNode PM.Node | WrapperInlineNode | WrapperBlockNode deriving (Show)

type PMTree = Tree PMTreeNode

pmDocFromPMTree :: PMTree -> PM.PMDoc
pmDocFromPMTree _ = undefined

groupedInlinesPandocTreeToPMTree :: Tree GroupedInlinesTree.DocNode -> PMTree
groupedInlinesPandocTreeToPMTree = fmap groupedInlinesTreeNodeToPMNode

groupedInlinesTreeNodeToPMNode :: GroupedInlinesTree.DocNode -> PMTreeNode
groupedInlinesTreeNodeToPMNode GroupedInlinesTree.Root = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "doc", PM.content = Nothing, PM.attrs = Nothing}
groupedInlinesTreeNodeToPMNode _ = undefined

leafTextSpansTreeNodeToPMNode :: LeafTextSpansTree.DocNode -> PMTreeNode
leafTextSpansTreeNodeToPMNode LeafTextSpansTree.Root = PMNode $ PM.BlockNode $ PM.PMBlock {PM.nodeType = "doc", PM.content = Nothing, PM.attrs = Nothing}
leafTextSpansTreeNodeToPMNode (LeafTextSpansTree.TreeNode (LeafTextSpansTree.BlockNode node)) = treeBlockNodeToPMBlockNode node
leafTextSpansTreeNodeToPMNode (LeafTextSpansTree.TreeNode (LeafTextSpansTree.InlineNode)) = WrapperInlineNode
leafTextSpansTreeNodeToPMNode (LeafTextSpansTree.TreeNode (LeafTextSpansTree.InlineContent textSpan)) = PMNode $ PM.TextNode $ treeTextSpanNodeToPMTextNode textSpan

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
