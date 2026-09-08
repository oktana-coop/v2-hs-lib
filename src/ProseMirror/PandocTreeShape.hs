-- Pandoc tree shapes reconciled with the ProseMirror model, one module per shape.
module ProseMirror.PandocTreeShape
  ( module ProseMirror.PandocTreeShape.HeadingAttrs,
    module ProseMirror.PandocTreeShape.HtmlComments,
    module ProseMirror.PandocTreeShape.ImageAlt,
    module ProseMirror.PandocTreeShape.ImplicitFigure,
    module ProseMirror.PandocTreeShape.OrderedListAttrs,
    module ProseMirror.PandocTreeShape.ListSpacing,
    reconcileWithProseMirrorModel,
    dropDetailsLostInProseMirror,
  )
where

import ProseMirror.PandocTreeShape.HeadingAttrs
import ProseMirror.PandocTreeShape.HtmlComments
import ProseMirror.PandocTreeShape.ImageAlt
import ProseMirror.PandocTreeShape.ImplicitFigure
import ProseMirror.PandocTreeShape.OrderedListAttrs
import ProseMirror.PandocTreeShape.ListSpacing
import Text.Pandoc.Definition (Pandoc)

-- What a Pandoc document goes through before it is mapped to ProseMirror nodes, by the conversion and by
-- the diff alike: implicit figures reconciled with ProseMirror's figure model, HTML comments dropped.
-- TODO: reconstruct supported raw HTML (e.g. `<figure>` with a caption) into native Pandoc
-- blocks here, so it flows through the normal conversion path instead of erroring downstream.
reconcileWithProseMirrorModel :: Pandoc -> Pandoc
reconcileWithProseMirrorModel = dropHtmlComments . stripCaptionEqualToAlt . wrapLoneImageInFigure

dropDetailsLostInProseMirror :: Pandoc -> Pandoc
dropDetailsLostInProseMirror = stringifyImageAlt . plainToPara . stripOrderedListAttrs . stripHeadingAttrs
