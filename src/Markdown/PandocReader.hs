module Markdown.PandocReader (readMarkdown) where

import qualified Data.Text as T
import Markdown.FrontMatter (frontMatterKeyOutline)
import Metadata.KeyOutline (addKeyOutline)
import Text.Pandoc (PandocMonad, ReaderOptions)
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Definition (Pandoc (..))

readMarkdown :: (PandocMonad m) => ReaderOptions -> T.Text -> m Pandoc
readMarkdown opts input = addFrontMatterKeyOutline <$> Pandoc.readMarkdown opts input
  where
    -- Pandoc's reader loses the order of the front matter keys, so it is read from the input separately.
    addFrontMatterKeyOutline :: Pandoc -> Pandoc
    addFrontMatterKeyOutline (Pandoc meta blocks) = Pandoc (addKeyOutline (frontMatterKeyOutline input) meta) blocks
