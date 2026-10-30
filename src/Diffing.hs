module Diffing (proseMirrorDiff, diffForProseMirror) where

import Conversion (readDocuments, toUnrepresentableContentError)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Tree (Tree (..))
import qualified DocTree.LeafTextSpans as PandocTree
import Error (Error)
import Format (Format)
import Metadata.KeyOutline (KeyOutline, addKeyOutline, stripTempMetaOutlineKey, takeKeyOutline)
import ProseMirror.Diff (DecoratedPMDoc, toDecoratedPMDoc)
import ProseMirror.PandocTreeShape (dropDetailsLostInProseMirror, findUnrepresentableContent, reconcileWithProseMirrorModel)
import RichTextDiff (DiffOptions, defaultDiffOptions, getAnnotatedTree)
import RichTextDiffOp (RichTextDiffOp)
import Text.Pandoc (Meta, Pandoc (..))
import Utils.Either (bothOrErrors)

proseMirrorDiff :: Format -> String -> String -> IO (Either (NonEmpty Error) DecoratedPMDoc)
proseMirrorDiff format beforeStr afterStr = do
  eitherDocs <- readDocuments format format beforeStr afterStr
  pure (eitherDocs >>= decoratedDiff)
  where
    decoratedDiff :: (Pandoc, Pandoc) -> Either (NonEmpty Error) DecoratedPMDoc
    decoratedDiff = fmap (uncurry toDecoratedPMDoc) . diffForProseMirror defaultDiffOptions

-- Diffs two documents as ProseMirror will show them: reconciled with its model, and without the details it
-- cannot represent.
--
-- The order of the metadata keys is left out of the diff, so a reorder alone is no metadata change. The diff comes
-- with the metadata of the "after" document for the diff's ProseMirror doc, with its key outline back, so the
-- "after" document is written with its keys in order.
diffForProseMirror :: DiffOptions -> (Pandoc, Pandoc) -> Either (NonEmpty Error) (Meta, Tree (RichTextDiffOp PandocTree.DocNode))
diffForProseMirror options (beforeDoc, afterDoc@(Pandoc afterMeta _)) = do
  (preparedBefore, preparedAfter@(Pandoc preparedAfterMeta _)) <- bothOrErrors (prepare beforeDoc) (prepare afterDoc)
  pure (addKeyOutline afterOutline preparedAfterMeta, getAnnotatedTree options preparedBefore preparedAfter)
  where
    afterOutline :: KeyOutline
    afterOutline = fst (takeKeyOutline afterMeta)

    prepare :: Pandoc -> Either Error Pandoc
    prepare = fmap dropDetailsLostInProseMirror . checkRepresentable . reconcileWithProseMirrorModel . withoutKeyOutline

    checkRepresentable :: Pandoc -> Either Error Pandoc
    checkRepresentable doc = case nonEmpty (findUnrepresentableContent doc) of
      Nothing -> Right doc
      Just unrepresentableContent -> Left (toUnrepresentableContentError unrepresentableContent)

    withoutKeyOutline :: Pandoc -> Pandoc
    withoutKeyOutline (Pandoc meta blocks) = Pandoc (stripTempMetaOutlineKey meta) blocks

