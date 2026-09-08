{-# LANGUAGE OverloadedStrings #-}

module ProseMirror.PandocTreeShape.HtmlComments (dropHtmlComments) where

import qualified Data.Text as T
import Text.Pandoc.Definition as Pandoc (Block (..), Format (..), Pandoc)
import Text.Pandoc.Walk (walk)

-- Pandoc represents HTML comments (notably its `<!-- -->` block separator) as raw blocks that
-- carry no document content. They have no ProseMirror representation but are safe to discard, so we
-- drop them before tree conversion. Any *other* raw block survives and becomes a conversion error.
dropHtmlComments :: Pandoc.Pandoc -> Pandoc.Pandoc
dropHtmlComments = walk (filter (not . isHtmlCommentBlock))
  where
    isHtmlCommentBlock :: Pandoc.Block -> Bool
    isHtmlCommentBlock (Pandoc.RawBlock format content) = isHtmlComment format content
    isHtmlCommentBlock _ = False

isHtmlComment :: Pandoc.Format -> T.Text -> Bool
isHtmlComment (Pandoc.Format formatName) content =
  formatName == "html" && "<!--" `T.isPrefixOf` strippedContent && "-->" `T.isSuffixOf` strippedContent
  where
    strippedContent = T.strip content
