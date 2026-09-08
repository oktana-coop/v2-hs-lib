module Patching (proseMirrorSteps) where

import Conversion (readDocuments)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import Data.Tree (Tree)
import Diff (diffForProseMirror)
import qualified DocTree.LeafTextSpans as PandocTree
import Format (Format (..))
import ProseMirror.Transform.FromDiff (TransformedPMDoc, toTransformedPMDoc)
import RichTextDiff (DiffOptions (..))
import RichTextDiffOp (RichTextDiffOp)
import Text.Pandoc (PandocError (PandocSomeError))

proseMirrorSteps :: Format -> Format -> String -> String -> IO (Either (NonEmpty PandocError) TransformedPMDoc)
proseMirrorSteps beforeFormat afterFormat beforeStr afterStr = do
  eitherDocs <- readDocuments beforeFormat afterFormat beforeStr afterStr
  pure $ eitherDocs >>= diffToPMSteps . diffForProseMirror diffOptions
  where
    -- Steps are exact edits, so a heavy reword must stay a set of small edits instead of collapsing into a
    -- whole-span replace.
    diffOptions :: DiffOptions
    diffOptions = DiffOptions {collapseRewrites = False}

    diffToPMSteps :: Tree (RichTextDiffOp PandocTree.DocNode) -> Either (NonEmpty PandocError) TransformedPMDoc
    diffToPMSteps = first (pure . toPandocError) . toTransformedPMDoc

    toPandocError :: String -> PandocError
    toPandocError = PandocSomeError . T.pack
