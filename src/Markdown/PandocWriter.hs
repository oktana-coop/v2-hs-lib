module Markdown.PandocWriter (writeNormalizedMarkdown) where

import qualified Data.Text as T
import Text.Pandoc (PandocMonad, WriterOptions, writeMarkdown)
import Text.Pandoc.Definition (Block (HorizontalRule, Para), Format (..), Inline (RawInline), Pandoc)
import Text.Pandoc.Walk (walk)

-- Wraps Pandoc's Markdown writer to canonicalize the output via project-local AST tweaks
-- applied just before serialization.
writeNormalizedMarkdown :: (PandocMonad m) => WriterOptions -> Pandoc -> m T.Text
writeNormalizedMarkdown opts = writeMarkdown opts . walk shortHr
  where
    -- Emit `---` instead of `writerColumns` dashes (default 72) without touching
    -- `writerColumns`, which also drives table layout and image alt-text wrapping.
    shortHr :: Block -> Block
    shortHr HorizontalRule = Para [RawInline (Format (T.pack "markdown")) (T.pack "---")]
    shortHr block = block
