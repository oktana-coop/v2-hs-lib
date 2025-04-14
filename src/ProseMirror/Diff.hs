module ProseMirror.Diff (proseMirrorDocWithDiffDecorations) where

import Data.Tree (Tree)
import DocTree.LeafTextSpans (DocNode)
import RichTextDiffOp (RichTextDiffOp)

proseMirrorDocWithDiffDecorations :: Tree (RichTextDiffOp DocNode) -> String
proseMirrorDocWithDiffDecorations = undefined