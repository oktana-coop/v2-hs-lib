{-# LANGUAGE FlexibleContexts #-}

module ProseMirror.PandocTreeShape.PlainParagraphs (plainToPara) where

import Text.Pandoc.Definition as Pandoc (Block (..))
import Text.Pandoc.Walk (Walkable, walk)

-- ProseMirror has only paragraphs, so Pandoc's `Plain` (a tight list item, a caption) and `Para` both map to
-- one. The readers disagree on which one a list item is: the Markdown reader gives `Para` in a loose list
-- while the ProseMirror reader compacts a single-paragraph item to `Plain`. Making every `Plain` a `Para`
-- lets an item read from either side compare equal.
--
-- TODO: model loose vs tight lists (e.g. a `tight` attribute on list nodes) so the writer emits `Plain` or
-- `Para` faithfully; this walk then retires.
plainToPara :: (Walkable Block t) => t -> t
plainToPara = walk toPara
  where
    toPara :: Block -> Block
    toPara (Pandoc.Plain inlines) = Pandoc.Para inlines
    toPara block = block
