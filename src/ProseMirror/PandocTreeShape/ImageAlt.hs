{-# LANGUAGE FlexibleContexts #-}

module ProseMirror.PandocTreeShape.ImageAlt (stringifyImageAlt) where

import Text.Pandoc.Definition as Pandoc (Inline (..))
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (Walkable, walk)

-- ProseMirror keeps an image's alt text as one string, which the ProseMirror reader turns into a single `Str`,
-- whereas the Markdown reader tokenizes it into words and spaces. Collapsing the alt inlines to their text
-- makes an image read from either side compare equal.
stringifyImageAlt :: (Walkable Inline t) => t -> t
stringifyImageAlt = walk stringifyAlt
  where
    stringifyAlt :: Inline -> Inline
    stringifyAlt (Pandoc.Image attr alt target)
      | null alt = Pandoc.Image attr [] target
      | otherwise = Pandoc.Image attr [Pandoc.Str (stringify alt)] target
    stringifyAlt inline = inline
