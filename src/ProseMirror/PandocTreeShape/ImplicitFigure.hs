{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- Pandoc ↔ ProseMirror bridge for Pandoc's implicit_figures extension.
--
-- Pandoc's implicit_figures option results in emitting a Figure when a paragraph with a single Image inline is encountered.
-- When this happens, the image's alt text appears (redundantly) in both the Figure's caption and the figure image's alt text (caption == alt).
-- A ProseMirror figure either has no caption (alt is the only carrier of the text) or has a user-authored one (coincidences excluded, it should be distinct from alt).
-- This is why we need functions which:
-- 1. Strip a Pandoc Figure's Caption before writing to ProseMirror:
-- [Pandoc] `Figure (Caption [Plain alt]) [Plain [Image alt]]` -> [Pandoc] `Figure (Caption []) [Plain [Image alt]]` -> [ProseMirror] `figure { figure_content { image } }`
-- 2. Add a Caption to a Figure that doesn't have one before writing to Pandoc. The Caption's text must be equal to the image's alt text.
-- [ProseMirror] `figure { figure_content { image } }` -> [Pandoc] `Figure (Caption []) [Plain [Image alt]]` -> `Figure (Caption [Plain alt]) [Plain [Image alt]]`
module ProseMirror.PandocTreeShape.ImplicitFigure (stripCaptionEqualToAlt, addCaptionEqualToAlt) where

import Text.Pandoc.Definition as Pandoc (Block (..), Caption (..), Inline (..))
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
