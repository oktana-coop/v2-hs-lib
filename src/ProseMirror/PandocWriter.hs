{-# LANGUAGE OverloadedStrings #-}

module ProseMirror.PandocWriter (writeProseMirror) where

import Control.Monad ((<=<))
import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Tree (Tree)
import qualified DocTree.GroupedInlines as GroupedInlinesTree (DocNode, toTree)
import qualified ProseMirror.Model as PM (PMDoc (..))
import ProseMirror.PandocTreeShape.FigureContent.GroupedInlines (unwrapFigureContentParaOrPlain)
import ProseMirror.PandocTreeShape.ImplicitFigure (stripCaptionEqualToAlt, wrapLoneImageInFigure)
import ProseMirror.Tree (groupedInlinesPandocTreeToPMTree, pmDocFromPMTree)
import Text.Pandoc (PandocError (PandocParseError), WriterOptions)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition as Pandoc (Block (RawBlock), Format (..), Pandoc)
import Text.Pandoc.Walk (walk)

writeProseMirror :: (PandocMonad m) => WriterOptions -> Pandoc.Pandoc -> m T.Text
-- `convertTreeToPMDoc` yields the PM doc inside the Pandoc monad, so we compose with Kleisli (`<=<`).
writeProseMirror _ = pure . pmDocToJSONText <=< convertTreeToPMDoc . buildDocTree . preprocessPandoc
  where
    -- TODO: reconstruct supported raw HTML (e.g. `<figure>` with a caption) into native Pandoc
    -- blocks here, so it flows through the normal conversion path instead of erroring downstream.
    preprocessPandoc :: Pandoc.Pandoc -> Pandoc.Pandoc
    preprocessPandoc = dropHtmlComments . stripCaptionEqualToAlt . wrapLoneImageInFigure

    buildDocTree :: Pandoc.Pandoc -> Tree GroupedInlinesTree.DocNode
    buildDocTree = unwrapFigureContentParaOrPlain . GroupedInlinesTree.toTree

    convertTreeToPMDoc :: (PandocMonad m) => Tree GroupedInlinesTree.DocNode -> m PM.PMDoc
    convertTreeToPMDoc = either handlePMConversionErrorMessage pure . (pmDocFromPMTree <=< groupedInlinesPandocTreeToPMTree)

    handlePMConversionErrorMessage :: (PandocMonad m) => String -> m a
    handlePMConversionErrorMessage = throwError . PandocParseError . T.pack

pmDocToJSONText :: PM.PMDoc -> T.Text
pmDocToJSONText = decodeUtf8 . BSL8.toStrict . encode

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
