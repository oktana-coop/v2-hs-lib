module Patching (proseMirrorSteps) where

import Conversion (readDocuments)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import Data.Tree (Tree)
import Diffing (diffForProseMirror)
import qualified DocTree.LeafTextSpans as PandocTree
import Error (Error (PatchError))
import Format (Format)
import ProseMirror.Transform.FromDiff (TransformedPMDoc, toTransformedPMDoc)
import RichTextDiff (DiffOptions (..))
import RichTextDiffOp (RichTextDiffOp)
import Text.Pandoc (Pandoc (..))

proseMirrorSteps :: Format -> Format -> String -> String -> IO (Either (NonEmpty Error) TransformedPMDoc)
proseMirrorSteps beforeFormat afterFormat beforeStr afterStr = do
  eitherDocs <- readDocuments beforeFormat afterFormat beforeStr afterStr
  pure (eitherDocs >>= steps)
  where
    steps :: (Pandoc, Pandoc) -> Either (NonEmpty Error) TransformedPMDoc
    steps docs = diffForProseMirror diffOptions docs >>= diffToPMSteps (metaDiffers docs)

    -- Steps are exact edits, so a heavy reword must stay a set of small edits instead of collapsing into a
    -- whole-span replace.
    diffOptions :: DiffOptions
    diffOptions = DiffOptions {collapseRewrites = False}

    -- The diff leaves out the order of the metadata keys, but the editor must still get a reorder: otherwise
    -- its next contribution would write the old order back. So the metadata is compared with its key outline.
    metaDiffers :: (Pandoc, Pandoc) -> Bool
    metaDiffers (Pandoc beforeMeta _, Pandoc afterMeta _) = beforeMeta /= afterMeta

    diffToPMSteps :: Bool -> Tree (RichTextDiffOp PandocTree.DocNode) -> Either (NonEmpty Error) TransformedPMDoc
    diffToPMSteps updatesMeta diff = first (pure . PatchError . T.pack) (toTransformedPMDoc updatesMeta diff)
