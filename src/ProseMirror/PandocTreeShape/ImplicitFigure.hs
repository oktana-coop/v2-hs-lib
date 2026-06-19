{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- Pandoc ↔ ProseMirror bridge for Pandoc's implicit_figures extension.
--
-- When implicit_figures is enabled, Pandoc emits a Figure for a lone image with non-empty alt, reusing the alt as the caption (so caption == alt).
-- Also, Pandoc emits a Paragraph with an inline image (not a Figure) when it encounters `![](url)` (no alt/caption) in Markdown.
-- In the ProseMirror model, a block-level image is always a figure with an optional caption.
-- This module's functions reconcile the above models.
module ProseMirror.PandocTreeShape.ImplicitFigure (stripCaptionEqualToAlt, addCaptionEqualToAlt, unwrapCaptionlessFigure, wrapLoneImageInFigure) where

import Text.Pandoc.Definition as Pandoc (Block (..), Caption (..), Inline (..), nullAttr)
import Text.Pandoc.Walk (Walkable, walk)

stripCaptionEqualToAlt :: (Walkable Block t) => t -> t
stripCaptionEqualToAlt = walk stripCaption
  where
    stripCaption :: Block -> Block
    stripCaption (Pandoc.Figure attr cap blocks) | capEqualToImageAlt cap blocks = Pandoc.Figure attr (Pandoc.Caption Nothing []) blocks
    stripCaption b = b

    capEqualToImageAlt :: Caption -> [Block] -> Bool
    capEqualToImageAlt (Pandoc.Caption Nothing [Pandoc.Plain capInlines]) [Pandoc.Plain [Pandoc.Image _ alt _]] =
      not (null alt) && capInlines == alt
    capEqualToImageAlt _ _ = False

addCaptionEqualToAlt :: (Walkable Block t) => t -> t
addCaptionEqualToAlt = walk addCaptionWhenAbsent
  where
    addCaptionWhenAbsent :: Block -> Block
    addCaptionWhenAbsent (Pandoc.Figure attr (Pandoc.Caption Nothing []) blocks)
      | Just alt <- findSingleImageAlt blocks,
        not (null alt) =
          Pandoc.Figure attr (Pandoc.Caption Nothing [Pandoc.Plain alt]) blocks
    addCaptionWhenAbsent b = b

    findSingleImageAlt :: [Block] -> Maybe [Inline]
    findSingleImageAlt [Pandoc.Plain [Pandoc.Image _ alt _]] = Just alt
    findSingleImageAlt _ = Nothing

-- A captionless figure whose image has no alt can't be written as a Markdown figure,
-- so unwrap it to a plain image that serializes to `![](url)`.
unwrapCaptionlessFigure :: (Walkable Block t) => t -> t
unwrapCaptionlessFigure = walk unwrap
  where
    unwrap :: Block -> Block
    unwrap (Pandoc.Figure _ (Pandoc.Caption Nothing []) [Pandoc.Plain [img@(Pandoc.Image _ [] _)]]) = Pandoc.Para [img]
    unwrap block = block

-- The inverse of unwrapCaptionlessFigure:
-- a lone image in a paragraph (`![](url)`, which Pandoc leaves as a plain image because its alt is empty) is wrapped into a captionless Figure.
wrapLoneImageInFigure :: (Walkable Block t) => t -> t
wrapLoneImageInFigure = walk wrap
  where
    wrap :: Block -> Block
    wrap (Pandoc.Para [img@(Pandoc.Image _ _ _)]) = Pandoc.Figure Pandoc.nullAttr (Pandoc.Caption Nothing []) [Pandoc.Plain [img]]
    wrap block = block
