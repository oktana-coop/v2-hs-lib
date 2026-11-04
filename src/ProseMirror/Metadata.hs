module ProseMirror.Metadata (addDocFrontMatter, docFrontMatter, writeDocFrontMatter) where

import Markdown.PandocWriter (writeFrontMatter)
import qualified ProseMirror.Model as PM (Block (..), BlockNode (..), FrontMatter, Node (..), PMDoc (..))
import ProseMirror.PandocTreeShape (dropUnrepresentableContent, reconcileWithProseMirrorModel)
import Text.Pandoc (PandocMonad, WriterOptions)
import Text.Pandoc.Definition (Meta, Pandoc (..))

-- The doc node with the front matter in its attribute. Other nodes are left as they are.
addDocFrontMatter :: Maybe PM.FrontMatter -> PM.Node -> PM.Node
addDocFrontMatter frontMatter (PM.BlockNode (PM.PMBlock (PM.Doc _) content)) = PM.BlockNode (PM.PMBlock (PM.Doc frontMatter) content)
addDocFrontMatter _ node = node

-- The front matter in the doc's attribute, if it has any.
docFrontMatter :: PM.PMDoc -> Maybe PM.FrontMatter
docFrontMatter (PM.PMDoc (PM.BlockNode (PM.PMBlock (PM.Doc frontMatter) _))) = frontMatter
docFrontMatter _ = Nothing

-- The front matter for the doc's attribute: the document's metadata, prepared and written as front matter.
writeDocFrontMatter :: (PandocMonad m) => WriterOptions -> Pandoc -> m (Maybe PM.FrontMatter)
writeDocFrontMatter opts = writeFrontMatter opts . metaOf . prepareForProseMirror . withoutBody
  where
    -- Reconciled with ProseMirror's model, without the content it can't hold.
    prepareForProseMirror :: Pandoc -> Pandoc
    prepareForProseMirror = dropUnrepresentableContent . reconcileWithProseMirrorModel

    -- Only the metadata is written, so only it is prepared.
    withoutBody :: Pandoc -> Pandoc
    withoutBody (Pandoc meta _) = Pandoc meta []

    metaOf :: Pandoc -> Meta
    metaOf (Pandoc meta _) = meta
