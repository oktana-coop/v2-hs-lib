{-# LANGUAGE OverloadedStrings #-}

module Markdown.PandocReader (readMarkdown, readFrontMatter) where

import qualified Data.Text as T
import Markdown.FrontMatter (frontMatterKeyOutline)
import Metadata.KeyOutline (addKeyOutline)
import Text.Pandoc (PandocMonad, ReaderOptions (readerExtensions))
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Definition (Meta, Pandoc (..))
import Text.Pandoc.Extensions (Extension (Ext_yaml_metadata_block), enableExtension)

readMarkdown :: (PandocMonad m) => ReaderOptions -> T.Text -> m Pandoc
readMarkdown opts input = addFrontMatterKeyOutline <$> Pandoc.readMarkdown opts input
  where
    -- Pandoc's reader loses the order of the front matter keys, so it is read from the input separately.
    addFrontMatterKeyOutline :: Pandoc -> Pandoc
    addFrontMatterKeyOutline (Pandoc meta blocks) = Pandoc (addKeyOutline (frontMatterKeyOutline input) meta) blocks

-- The metadata in front matter YAML, read as the Markdown reader reads a document's front matter.
readFrontMatter :: (PandocMonad m) => ReaderOptions -> T.Text -> m Meta
readFrontMatter opts yaml = do
  Pandoc meta _ <- readMarkdown optsWithMetadataBlock (T.unlines ("---" : T.lines yaml <> ["---"]))
  pure meta
  where
    optsWithMetadataBlock = opts {readerExtensions = enableExtension Ext_yaml_metadata_block (readerExtensions opts)}
