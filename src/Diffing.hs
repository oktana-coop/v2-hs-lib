module Diffing (proseMirrorDiff, diffForProseMirror) where

import Conversion (pandocWriterOptions, readDocuments, toUnrepresentableContentError, toWriteError)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Tree (Tree (..))
import Error (Error)
import Format (Format (ProseMirror))
import Markdown.PandocWriter (writeFrontMatter)
import Metadata.KeyOutline (KeyOutline, addKeyOutline, stripTempMetaOutlineKey, takeKeyOutline)
import NodeDiff.LeafTextSpans (DocNodeDiff)
import ProseMirror.Diff (DecoratedPMDoc, toDecoratedPMDoc)
import qualified ProseMirror.Model as PM (FrontMatter)
import ProseMirror.PandocTreeShape (dropDetailsLostInProseMirror, findUnrepresentableContent, reconcileWithProseMirrorModel)
import RichTextDiff (DiffOptions, defaultDiffOptions, getAnnotatedTree)
import Text.Pandoc (Meta, Pandoc (..), runPure)
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
-- with the front matter of the "after" document for the diff's ProseMirror doc, written with its keys in order.
diffForProseMirror :: DiffOptions -> (Pandoc, Pandoc) -> Either (NonEmpty Error) (Maybe PM.FrontMatter, Tree DocNodeDiff)
diffForProseMirror options (beforeDoc, afterDoc@(Pandoc afterMeta _)) = do
  (preparedBefore, preparedAfter@(Pandoc preparedAfterMeta _)) <- bothOrErrors (prepare beforeDoc) (prepare afterDoc)
  afterFrontMatter <- first pure (toFrontMatter (addKeyOutline afterOutline preparedAfterMeta))
  pure (afterFrontMatter, getAnnotatedTree options preparedBefore preparedAfter)
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

    toFrontMatter :: Meta -> Either Error (Maybe PM.FrontMatter)
    toFrontMatter = first (toWriteError ProseMirror) . runPure . writeFrontMatter pandocWriterOptions
