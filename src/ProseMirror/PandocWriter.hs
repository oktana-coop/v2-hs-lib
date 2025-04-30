module ProseMirror.PandocWriter (writeProseMirror) where

import qualified Data.Text as T
import qualified DocTree.LeafTextSpans as PandocTree
import ProseMirror.PMTree (PMTree)
import Text.Pandoc (WriterOptions)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition as Pandoc (Pandoc (Pandoc))

writeProseMirror :: (PandocMonad m) => WriterOptions -> Pandoc.Pandoc -> m T.Text
writeProseMirror _ (Pandoc.Pandoc _ _) = undefined

toProseMirrorTree :: PandocTree.DocNode -> PMTree
toProseMirrorTree = undefined
