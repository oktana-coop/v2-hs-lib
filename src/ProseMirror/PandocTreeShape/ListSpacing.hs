{-# LANGUAGE FlexibleContexts #-}

module ProseMirror.PandocTreeShape.ListSpacing (plainToPara) where

import Text.Pandoc.Definition as Pandoc (Block (..))
import Text.Pandoc.Walk (Walkable, walk)

-- The editor does not distinguish tight lists from loose ones: it has only paragraphs and writes
-- single-paragraph items tight. So Pandoc's `Plain` and `Para` are the same block to it, and reading
-- them as one keeps a document read from various sources comparable.
plainToPara :: (Walkable Block t) => t -> t
plainToPara = walk toPara
  where
    toPara :: Block -> Block
    toPara (Pandoc.Plain inlines) = Pandoc.Para inlines
    toPara block = block
