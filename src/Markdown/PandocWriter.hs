{-# LANGUAGE OverloadedStrings #-}

module Markdown.PandocWriter (writeNormalizedMarkdown, writeFrontMatter) where

import Data.Maybe (isJust)
import qualified Data.Text as T
import Markdown.FrontMatter (frontMatterText, invalidFrontMatter, orderFrontMatter, wrapInFrontMatterDelimiters)
import Metadata.KeyOutline (takeKeyOutline)
import Text.Pandoc (PandocMonad, WriterOptions (writerExtensions, writerTemplate), writeMarkdown)
import Text.Pandoc.Definition (Block (HorizontalRule, Para), Format (..), Inline (RawInline), Meta, Pandoc (..), nullMeta)
import Text.Pandoc.Extensions (Extension (Ext_yaml_metadata_block), enableExtension)
import Text.Pandoc.Templates (compileDefaultTemplate)
import Text.Pandoc.Walk (walk)

-- Wraps Pandoc's Markdown writer to canonicalize the output via project-local AST tweaks
-- applied just before serialization, and to keep the order of the metadata keys.
writeNormalizedMarkdown :: (PandocMonad m) => WriterOptions -> Pandoc -> m T.Text
writeNormalizedMarkdown opts (Pandoc meta blocks) = case invalidFrontMatter meta of
  Just yaml -> writeKeepingInvalidFrontMatter yaml
  Nothing -> writeInKeyOrder
  where
    -- The keys of the front matter Pandoc writes are put back in the order of the key outline.
    writeInKeyOrder :: (PandocMonad m) => m T.Text
    writeInKeyOrder = orderFrontMatter keyOutline <$> writeWithMeta metaWithoutKeyOutline
      where
        (keyOutline, metaWithoutKeyOutline) = takeKeyOutline meta

    writeKeepingInvalidFrontMatter :: (PandocMonad m) => T.Text -> m T.Text
    writeKeepingInvalidFrontMatter yaml = withRawFrontMatter yaml <$> writeWithMeta nullMeta

    withRawFrontMatter :: T.Text -> T.Text -> T.Text
    withRawFrontMatter yaml body
      -- Only with a template, as Pandoc writes metadata only with one.
      | isJust (writerTemplate opts) = wrapInFrontMatterDelimiters yaml <> "\n" <> body
      | otherwise = body

    writeWithMeta :: (PandocMonad m) => Meta -> m T.Text
    writeWithMeta documentMeta = writeMarkdown opts (walk shortHr (Pandoc documentMeta blocks))

    -- Emit `---` instead of `writerColumns` dashes (default 72) without touching
    -- `writerColumns`, which also drives table layout and image alt-text wrapping.
    shortHr :: Block -> Block
    shortHr HorizontalRule = Para [RawInline (Format (T.pack "markdown")) (T.pack "---")]
    shortHr block = block

-- The metadata as front matter YAML, written as the Markdown writer writes a document's front matter, or nothing
-- if it writes none.
writeFrontMatter :: (PandocMonad m) => WriterOptions -> Meta -> m (Maybe T.Text)
writeFrontMatter opts meta = do
  -- Without a template, Pandoc's Markdown writer writes no metadata.
  template <- compileDefaultTemplate "markdown"
  markdown <- writeNormalizedMarkdown opts {writerTemplate = Just template, writerExtensions = enableExtension Ext_yaml_metadata_block (writerExtensions opts)} (Pandoc meta [])
  pure (frontMatterText markdown >>= nonEmptyYaml)
  where
    -- Pandoc writes an empty block for metadata whose values are all empty.
    nonEmptyYaml :: T.Text -> Maybe T.Text
    nonEmptyYaml yaml = if T.null yaml then Nothing else Just yaml
