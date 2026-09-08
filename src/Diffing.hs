module Diffing (proseMirrorDiff, diffForProseMirror) where

import Conversion (readDocuments)
import Data.List.NonEmpty (NonEmpty)
import Data.Tree (Tree)
import qualified DocTree.LeafTextSpans as PandocTree
import Format (Format (..))
import ProseMirror.Diff (DecoratedPMDoc, toDecoratedPMDoc)
import ProseMirror.PandocTreeShape (dropDetailsLostInProseMirror, reconcileWithProseMirrorModel)
import RichTextDiff (DiffOptions, defaultDiffOptions, getAnnotatedTree)
import RichTextDiffOp (RichTextDiffOp)
import Text.Pandoc (Pandoc, PandocError)

proseMirrorDiff :: Format -> String -> String -> IO (Either (NonEmpty PandocError) DecoratedPMDoc)
proseMirrorDiff format doc1Str doc2Str = (fmap . fmap) (toDecoratedPMDoc . diffForProseMirror defaultDiffOptions) (readDocuments format format doc1Str doc2Str)

-- Diffs two documents as ProseMirror will show them: reconciled with its model, and without the details it
-- cannot represent.
diffForProseMirror :: DiffOptions -> (Pandoc, Pandoc) -> Tree (RichTextDiffOp PandocTree.DocNode)
diffForProseMirror options (doc1, doc2) = getAnnotatedTree options (prepare doc1) (prepare doc2)
  where
    prepare :: Pandoc -> Pandoc
    prepare = dropDetailsLostInProseMirror . reconcileWithProseMirrorModel
