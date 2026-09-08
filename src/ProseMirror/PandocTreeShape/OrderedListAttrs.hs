{-# LANGUAGE FlexibleContexts #-}

module ProseMirror.PandocTreeShape.OrderedListAttrs (stripOrderedListAttrs) where

import Text.Pandoc.Definition as Pandoc (Block (..), ListNumberDelim (DefaultDelim), ListNumberStyle (DefaultStyle))
import Text.Pandoc.Walk (Walkable, walk)

-- The ProseMirror model carries an ordered list's start number but not its numbering style or delimiter, so
-- the ProseMirror reader always yields the defaults for those, whereas the Markdown reader records what it
-- saw (`1.` gives `Decimal` and `Period`). Resetting them makes a list read from either side compare equal.
-- TODO: give the numbering style and delimiter a place in the ProseMirror model; this walk then retires.
stripOrderedListAttrs :: (Walkable Block t) => t -> t
stripOrderedListAttrs = walk strip
  where
    strip :: Block -> Block
    strip (Pandoc.OrderedList (start, _, _) items) = Pandoc.OrderedList (start, DefaultStyle, DefaultDelim) items
    strip block = block
