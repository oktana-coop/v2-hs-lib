{-# LANGUAGE FlexibleContexts #-}

module ProseMirror.PandocTreeShape.OrderedListAttrs (stripOrderedListAttrs) where

import Text.Pandoc.Definition as Pandoc (Block (..), ListNumberDelim (DefaultDelim), ListNumberStyle (DefaultStyle))
import Text.Pandoc.Walk (Walkable, walk)

-- The ProseMirror schema defines no start number, numbering style or delimiter for an ordered list, so
-- the ProseMirror reader always yields the defaults, whereas the Markdown reader records what Pandoc can express.
-- Resetting the attributes makes a list read from either side compare equal.
--
-- TODO: map the start number to the editor's `order` attribute (which exists in the schema but not in the
-- model here) and give the numbering style and delimiter a place too; this walk then retires.
stripOrderedListAttrs :: (Walkable Block t) => t -> t
stripOrderedListAttrs = walk strip
  where
    strip :: Block -> Block
    strip (Pandoc.OrderedList _ items) = Pandoc.OrderedList (1, DefaultStyle, DefaultDelim) items
    strip block = block
