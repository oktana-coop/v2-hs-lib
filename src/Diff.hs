module Diff (proseMirrorDiff, proseMirrorSteps) where

import Conversion (pandocReaderOptions, readFrom)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import Data.Tree (Tree)
import qualified DocTree.LeafTextSpans as PandocTree
import Format (Format (..))
import ProseMirror.Diff (DecoratedPMDoc, toDecoratedPMDoc)
import ProseMirror.PandocTreeShape.ImplicitFigure (stripCaptionEqualToAlt, wrapLoneImageInFigure)
import ProseMirror.Transform.FromDiff (TransformedPMDoc, toTransformedPMDoc)
import RichTextDiff (DiffOptions (..), defaultDiffOptions, getAnnotatedTree)
import RichTextDiffOp (RichTextDiffOp)
import Text.Pandoc (Pandoc, PandocError (PandocSomeError))
import Text.Pandoc.Class (runIO)

proseMirrorDiff :: Format -> String -> String -> IO (Either (NonEmpty PandocError) DecoratedPMDoc)
proseMirrorDiff format doc1Str doc2Str = (fmap . fmap) toDecoratedPMDoc (pandocDiff defaultDiffOptions format format doc1Str doc2Str)

proseMirrorSteps :: Format -> Format -> String -> String -> IO (Either (NonEmpty PandocError) TransformedPMDoc)
proseMirrorSteps beforeFormat afterFormat beforeStr afterStr = do
  eitherDiffTree <- pandocDiff DiffOptions {collapseRewrites = False} beforeFormat afterFormat beforeStr afterStr
  pure $ eitherDiffTree >>= diffToPMSteps
  where
    diffToPMSteps :: Tree (RichTextDiffOp PandocTree.DocNode) -> Either (NonEmpty PandocError) TransformedPMDoc
    diffToPMSteps = first (pure . toPandocError) . toTransformedPMDoc

    toPandocError :: String -> PandocError
    toPandocError = PandocSomeError . T.pack

-- Reads both sides (each in its own format) and diffs them as Pandoc trees.
pandocDiff :: DiffOptions -> Format -> Format -> String -> String -> IO (Either (NonEmpty PandocError) (Tree (RichTextDiffOp PandocTree.DocNode)))
pandocDiff options format1 format2 doc1Str doc2Str = do
  eitherDoc1 <- runIO $ readFrom format1 pandocReaderOptions (T.pack doc1Str)
  eitherDoc2 <- runIO $ readFrom format2 pandocReaderOptions (T.pack doc2Str)

  pure $ diffDocs <$> bothOrErrors eitherDoc1 eitherDoc2
  where
    diffDocs :: (Pandoc, Pandoc) -> Tree (RichTextDiffOp PandocTree.DocNode)
    diffDocs (doc1, doc2) = getAnnotatedTree options (preprocess doc1) (preprocess doc2)

    preprocess :: Pandoc -> Pandoc
    preprocess = stripCaptionEqualToAlt . wrapLoneImageInFigure

-- Return both results, or the errors of both sides.
bothOrErrors :: Either e a -> Either e b -> Either (NonEmpty e) (a, b)
bothOrErrors (Left err1) (Left err2) = Left (err1 :| [err2])
bothOrErrors (Left err1) _ = Left (err1 :| [])
bothOrErrors _ (Left err2) = Left (err2 :| [])
bothOrErrors (Right a) (Right b) = Right (a, b)
