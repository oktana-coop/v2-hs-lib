{-# LANGUAGE FlexibleContexts #-}

module ProseMirror.PandocTreeShape.HeadingAttrs (stripHeadingAttrs) where

import Text.Pandoc.Definition as Pandoc (Block (..), nullAttr)
import Text.Pandoc.Walk (Walkable, walk)

-- A ProseMirror heading carries only its level, whereas some Pandoc readers (e.g. Markdown) also derive an
-- identifier from the heading text (via `Ext_auto_identifiers`). Stripping the attributes makes a heading read
-- from either side compare equal.
--
-- TODO: carry Pandoc attributes on ProseMirror nodes (an opaque `pandocAttr`, like `pandocMeta` on the doc) and
-- read Markdown without auto identifiers, so explicit attributes round-trip; this walk then retires.
stripHeadingAttrs :: (Walkable Block t) => t -> t
stripHeadingAttrs = walk strip
  where
    strip :: Block -> Block
    strip (Pandoc.Header level _ inlines) = Pandoc.Header level nullAttr inlines
    strip block = block
