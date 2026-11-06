{-# LANGUAGE OverloadedStrings #-}

module Markdown.PandocReader (readMarkdown, readFrontMatter, readFrontMatterKeepingInvalid) where

import Control.Monad.Except (catchError, throwError)
import qualified Data.Text as T
import Markdown.FrontMatter (frontMatterKeyOutline, pandocWithInvalidFrontMatter, splitFrontMatter, wrapInFrontMatterDelimiters)
import Metadata.KeyOutline (addKeyOutline)
import Text.Pandoc (PandocError, PandocMonad, ReaderOptions (readerExtensions))
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Definition (Meta, Pandoc (..), isNullMeta)
import Text.Pandoc.Extensions (Extension (Ext_yaml_metadata_block), enableExtension)
import Utils.Pandoc (isYamlMetadataError)

readMarkdown :: (PandocMonad m) => ReaderOptions -> T.Text -> m Pandoc
readMarkdown opts input = readMarkdownAddingKeyOutline opts input `catchError` recoverInvalidFrontMatter
  where
    recoverInvalidFrontMatter :: (PandocMonad m) => PandocError -> m Pandoc
    recoverInvalidFrontMatter err = case splitFrontMatter input of
      Just (yaml, rest) | isYamlMetadataError err -> do
        -- Attempt reading again, without the invalid frontmatter.
        Pandoc restMeta blocks <- Pandoc.readMarkdown opts rest `catchError` const rethrow
        if isNullMeta restMeta
          then pure (pandocWithInvalidFrontMatter yaml blocks)
          -- The raw text would leave out the metadata from YAML blocks further down, and all of the document's
          -- metadata must be kept.
          else rethrow
      _ -> rethrow
      where
        rethrow :: (PandocMonad m) => m a
        rethrow = throwError err

-- The metadata in front matter YAML, read as the Markdown reader reads a document's front matter. YAML Pandoc
-- can't read is an error.
readFrontMatter :: (PandocMonad m) => ReaderOptions -> T.Text -> m Meta
readFrontMatter = readFrontMatterWith readMarkdownAddingKeyOutline

-- The same, keeping YAML Pandoc can't read as its raw text.
readFrontMatterKeepingInvalid :: (PandocMonad m) => ReaderOptions -> T.Text -> m Meta
readFrontMatterKeepingInvalid = readFrontMatterWith readMarkdown

readMarkdownAddingKeyOutline :: (PandocMonad m) => ReaderOptions -> T.Text -> m Pandoc
readMarkdownAddingKeyOutline opts input = addFrontMatterKeyOutline <$> Pandoc.readMarkdown opts input
  where
    -- Pandoc's reader loses the order of the front matter keys, so it is read from the input separately.
    addFrontMatterKeyOutline :: Pandoc -> Pandoc
    addFrontMatterKeyOutline (Pandoc meta blocks) = Pandoc (addKeyOutline (frontMatterKeyOutline input) meta) blocks

-- Reads the YAML as a document of front matter alone, with the given reader.
readFrontMatterWith :: (PandocMonad m) => (ReaderOptions -> T.Text -> m Pandoc) -> ReaderOptions -> T.Text -> m Meta
readFrontMatterWith readDocument opts yaml = do
  Pandoc meta _ <- readDocument optsWithMetadataBlock (wrapInFrontMatterDelimiters yaml)
  pure meta
  where
    optsWithMetadataBlock = opts {readerExtensions = enableExtension Ext_yaml_metadata_block (readerExtensions opts)}
