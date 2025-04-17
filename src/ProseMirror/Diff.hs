module ProseMirror.Diff (proseMirrorDocWithDiffDecorations) where

import Control.Monad.State (State, evalState)
import qualified Data.Text as T
import Data.Tree (Tree (..))
import DocTree.LeafTextSpans (DocNode)
import qualified ProseMirror.PMJson as PM (Node (..))
import RichTextDiffOp (RichTextDiffOp)

data DecorationAttrs = DecorationAttrs {nodeName :: Maybe T.Text, cssClass :: Maybe T.Text, style :: Maybe T.Text} deriving (Show, Eq)

data InlineDecoration a = PMInlineDecoration {from :: Int, to :: Int, attrs :: DecorationAttrs} deriving (Show, Eq)

data WidgetDecoration a = PMWidgetDecoration {pos :: Int} deriving (Show, Eq)

data Decoration a = InlineDecoration (InlineDecoration a) | WidgetDecoration (WidgetDecoration a) deriving (Show, Eq)

type DecoratedPMTree = Tree (Either PM.Node (Decoration PM.Node))

type PMIndex = Int

proseMirrorDocWithDiffDecorations :: Tree (RichTextDiffOp DocNode) -> DecoratedPMTree
proseMirrorDocWithDiffDecorations diffTree = evalState (walkDiffTree diffTree) 0

walkDiffTree :: Tree (RichTextDiffOp DocNode) -> State PMIndex DecoratedPMTree
walkDiffTree (Node nodeWithDiff subTrees) = do
  pmNode <- walkDiffTreeNode nodeWithDiff
  pmSubForest <- mapM (walkDiffTree) subTrees
  pure $ Node {rootLabel = pmNode, subForest = pmSubForest}

walkDiffTreeNode :: RichTextDiffOp DocNode -> State PMIndex (Either PM.Node (Decoration PM.Node))
walkDiffTreeNode = undefined
