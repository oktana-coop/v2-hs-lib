{-# LANGUAGE OverloadedStrings #-}

module Diffing (ProseMirrorDiff, proseMirrorDiff, diffForProseMirror) where

import Conversion (pandocWriterOptions, readDocuments, toUnrepresentableContentError, toWriteError)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (isJust)
import Data.Tree (Tree (..))
import Error (Error)
import Format (Format (ProseMirror))
import Markdown.FrontMatter (invalidFrontMatter)
import Metadata.Diff (metadataDiffToJSON)
import Metadata.KeyOutline (stripTempMetaOutlineKey)
import NodeDiff.LeafTextSpans (DocNodeDiff)
import ProseMirror.Diff (DecoratedPMDoc, toDecoratedPMDoc)
import qualified ProseMirror.Diff as DecoratedPMDoc (DecoratedPMDoc (..))
import ProseMirror.Metadata (writeDocFrontMatter)
import qualified ProseMirror.Model as PM (FrontMatter)
import ProseMirror.PandocTreeShape (dropDetailsLostInProseMirror, findUnrepresentableContent, reconcileWithProseMirrorModel)
import RichTextDiff (DiffOptions, defaultDiffOptions, getAnnotatedTree)
import Text.Pandoc (Pandoc (..), PandocPure, runPure)
import Utils.Either (bothOrErrors)

-- The diff as ProseMirror shows it, with the metadata's diff, which the doc doesn't hold. There is no metadata diff
-- when either document's front matter is invalid.
data ProseMirrorDiff = ProseMirrorDiff DecoratedPMDoc (Maybe (Tree DocNodeDiff))

instance ToJSON ProseMirrorDiff where
  toJSON (ProseMirrorDiff decoratedPMDoc maybeDiff) =
    object
      [ "doc" .= DecoratedPMDoc.doc decoratedPMDoc,
        "decorations" .= DecoratedPMDoc.decorations decoratedPMDoc,
        "metadataDiff" .= fmap metadataDiffToJSON maybeDiff
      ]

proseMirrorDiff :: Format -> String -> String -> IO (Either (NonEmpty Error) ProseMirrorDiff)
proseMirrorDiff format beforeStr afterStr = do
  eitherDocs <- readDocuments format format beforeStr afterStr
  pure (eitherDocs >>= decoratedDiff)
  where
    decoratedDiff :: (Pandoc, Pandoc) -> Either (NonEmpty Error) ProseMirrorDiff
    decoratedDiff docs = do
      (afterFrontMatter, diff) <- diffForProseMirror defaultDiffOptions docs
      pure (ProseMirrorDiff (toDecoratedPMDoc afterFrontMatter diff) (metadataDiffFor docs diff))

    -- Front matter Pandoc couldn't read has no metadata to compare, so either side's makes the metadata diff
    -- meaningless.
    metadataDiffFor :: (Pandoc, Pandoc) -> Tree DocNodeDiff -> Maybe (Tree DocNodeDiff)
    metadataDiffFor (beforeDoc, afterDoc) diff
      | any hasInvalidFrontMatter [beforeDoc, afterDoc] = Nothing
      | otherwise = Just diff

    hasInvalidFrontMatter :: Pandoc -> Bool
    hasInvalidFrontMatter (Pandoc meta _) = isJust (invalidFrontMatter meta)

-- Diffs two documents as ProseMirror will show them: reconciled with its model, and without the details it
-- cannot represent.
--
-- The order of the metadata keys is left out of the diff, so a reorder alone is no metadata change. The diff comes
-- with the front matter of the "after" document for the diff's ProseMirror doc, with its keys in order. The diff's
-- preparation is for the comparison only.
diffForProseMirror :: DiffOptions -> (Pandoc, Pandoc) -> Either (NonEmpty Error) (Maybe PM.FrontMatter, Tree DocNodeDiff)
diffForProseMirror options (beforeDoc, afterDoc) = do
  (preparedBefore, preparedAfter) <- bothOrErrors (prepare beforeDoc) (prepare afterDoc)
  afterFrontMatter <- first pure (docToFrontMatter afterDoc)
  pure (afterFrontMatter, getAnnotatedTree options preparedBefore preparedAfter)
  where
    prepare :: Pandoc -> Either Error Pandoc
    prepare = fmap dropDetailsLostInProseMirror . checkRepresentable . reconcileWithProseMirrorModel . withoutKeyOutline

    checkRepresentable :: Pandoc -> Either Error Pandoc
    checkRepresentable doc = case nonEmpty (findUnrepresentableContent doc) of
      Nothing -> Right doc
      Just unrepresentableContent -> Left (toUnrepresentableContentError unrepresentableContent)

    withoutKeyOutline :: Pandoc -> Pandoc
    withoutKeyOutline (Pandoc meta blocks) = Pandoc (stripTempMetaOutlineKey meta) blocks

    docToFrontMatter :: Pandoc -> Either Error (Maybe PM.FrontMatter)
    docToFrontMatter = runPandocPure . writeDocFrontMatter pandocWriterOptions

    -- Runs a Pandoc computation without IO; its errors are write errors of the ProseMirror output.
    runPandocPure :: PandocPure a -> Either Error a
    runPandocPure = first (toWriteError ProseMirror) . runPure
