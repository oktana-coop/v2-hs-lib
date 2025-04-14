module ProseMirror.Diff (proseMirrorDocWithDiffDecorations) where

import qualified Data.Text as T
import Data.Tree (Tree)
import DocTree.LeafTextSpans (DocNode)
import ProseMirror.PMJson (Doc, Node (..))
import RichTextDiffOp (RichTextDiffOp)

data DecorationAttrs = DecorationAttrs {nodeName :: Maybe T.Text, cssClass :: Maybe T.Text, style :: Maybe T.Text} deriving (Show, Eq)

data InlineDecoration = PMInlineDecoration {from :: Int, to :: Int, attrs :: DecorationAttrs} deriving (Show, Eq)

data WidgetDecoration = PMWidgetDecoration {pos :: Int, node :: Node} deriving (Show, Eq)

data Decoration = InlineDecoration InlineDecoration | WidgetDecoration WidgetDecoration deriving (Show, Eq)

data DocWithDecorations = DocWithDecorations {doc :: Doc, decorations :: [Decoration]} deriving (Show, Eq)

proseMirrorDocWithDiffDecorations :: Tree (RichTextDiffOp DocNode) -> DocWithDecorations
proseMirrorDocWithDiffDecorations = undefined