module ProseMirror.Diff (proseMirrorDocWithDiffDecorations) where

import Data.Tree (Tree)
import Diff.RichTextDiffOp
import DocTree.LeafTextSpans (DocNode)

proseMirrorDocWithDiffDecorations :: Tree (RichTextDiffOp DocNode) -> String
proseMirrorDocWithDiffDecorations = undefined