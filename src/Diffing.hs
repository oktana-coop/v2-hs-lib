module Diffing (proseMirrorDiff, diffForProseMirror) where

import Conversion (readDocuments)
import Data.List.NonEmpty (NonEmpty)
import Data.Tree (Tree (..))
import qualified DocTree.LeafTextSpans as PandocTree
import Error (Error)
import Format (Format)
import Metadata.KeyOutline (KeyOutline, addKeyOutline, stripTempMetaOutlineKey, takeKeyOutline)
import ProseMirror.Diff (DecoratedPMDoc, toDecoratedPMDoc)
import ProseMirror.PandocTreeShape (dropDetailsLostInProseMirror, reconcileWithProseMirrorModel)
import RichTextDiff (DiffOptions, defaultDiffOptions, getAnnotatedTree)
import RichTextDiffOp (RichTextDiffOp)
import Text.Pandoc (Pandoc (..))

proseMirrorDiff :: Format -> String -> String -> IO (Either (NonEmpty Error) DecoratedPMDoc)
proseMirrorDiff format beforeStr afterStr = (fmap . fmap) (toDecoratedPMDoc . diffForProseMirror defaultDiffOptions) (readDocuments format format beforeStr afterStr)

-- Diffs two documents as ProseMirror will show them: reconciled with its model, and without the details it
-- cannot represent.
--
-- The order of the metadata keys is left out of the diff, so a reorder alone is no metadata change. The root of
-- the diff holds the metadata of the "after" document, so it gets its key outline back, and the "after" document
-- is written with its keys in order.
diffForProseMirror :: DiffOptions -> (Pandoc, Pandoc) -> Tree (RichTextDiffOp PandocTree.DocNode)
diffForProseMirror options (beforeDoc, afterDoc@(Pandoc afterMeta _)) = withRootKeyOutline afterOutline (getAnnotatedTree options (prepare beforeDoc) (prepare afterDoc))
  where
    afterOutline :: KeyOutline
    afterOutline = fst (takeKeyOutline afterMeta)

    prepare :: Pandoc -> Pandoc
    prepare = dropDetailsLostInProseMirror . reconcileWithProseMirrorModel . withoutKeyOutline

    withoutKeyOutline :: Pandoc -> Pandoc
    withoutKeyOutline (Pandoc meta blocks) = Pandoc (stripTempMetaOutlineKey meta) blocks

    withRootKeyOutline :: KeyOutline -> Tree (RichTextDiffOp PandocTree.DocNode) -> Tree (RichTextDiffOp PandocTree.DocNode)
    withRootKeyOutline outline (Node rootNodeWithDiffOp children) = Node (fmap addOutlineToRoot rootNodeWithDiffOp) children
      where
        addOutlineToRoot :: PandocTree.DocNode -> PandocTree.DocNode
        addOutlineToRoot (PandocTree.Root meta) = PandocTree.Root (addKeyOutline outline meta)
        addOutlineToRoot node = node
