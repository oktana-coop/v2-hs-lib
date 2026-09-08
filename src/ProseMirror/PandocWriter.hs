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
import ProseMirror.PandocTreeShape (reconcileWithProseMirrorModel)
import ProseMirror.PandocTreeShape.FigureContent.GroupedInlines (unwrapFigureContentParaOrPlain)
import ProseMirror.Tree (groupedInlinesPandocTreeToPMTree, pmDocFromPMTree)
import Text.Pandoc (PandocError (PandocParseError), WriterOptions)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition as Pandoc (Pandoc)

writeProseMirror :: (PandocMonad m) => WriterOptions -> Pandoc.Pandoc -> m T.Text
-- `convertTreeToPMDoc` yields the PM doc inside the Pandoc monad, so we compose with Kleisli (`<=<`).
writeProseMirror _ = pure . pmDocToJSONText <=< convertTreeToPMDoc . buildDocTree . reconcileWithProseMirrorModel
  where
    buildDocTree :: Pandoc.Pandoc -> Tree GroupedInlinesTree.DocNode
    buildDocTree = unwrapFigureContentParaOrPlain . GroupedInlinesTree.toTree

    convertTreeToPMDoc :: (PandocMonad m) => Tree GroupedInlinesTree.DocNode -> m PM.PMDoc
    convertTreeToPMDoc = either handlePMConversionErrorMessage pure . (pmDocFromPMTree <=< groupedInlinesPandocTreeToPMTree)

    handlePMConversionErrorMessage :: (PandocMonad m) => String -> m a
    handlePMConversionErrorMessage = throwError . PandocParseError . T.pack

pmDocToJSONText :: PM.PMDoc -> T.Text
pmDocToJSONText = decodeUtf8 . BSL8.toStrict . encode
