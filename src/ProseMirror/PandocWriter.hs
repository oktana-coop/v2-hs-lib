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
import ProseMirror.Error (PMConversionError (..))
import ProseMirror.Metadata (addDocMeta)
import qualified ProseMirror.Model as PM (PMDoc (..))
import ProseMirror.PandocTreeShape (dropUnrepresentableContent, findUnrepresentableBlocks, findUnrepresentableInlines, reconcileWithProseMirrorModel)
import ProseMirror.PandocTreeShape.FigureContent.GroupedInlines (unwrapFigureContentParaOrPlain)
import ProseMirror.PandocTreeShape.Metadata.GroupedInlines (withoutMetadataSubtree)
import ProseMirror.Tree (groupedInlinesPandocTreeToPMTree, pmDocFromPMTree)
import Text.Pandoc (PandocError (PandocShouldNeverHappenError, PandocSomeError), WriterOptions)
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Definition as Pandoc (Pandoc (..))
import Text.Pandoc.Logging (LogMessage (BlockNotRendered, InlineNotRendered))

writeProseMirror :: (PandocMonad m) => WriterOptions -> Pandoc.Pandoc -> m T.Text
-- Some steps yield their result inside the Pandoc monad, so we compose with Kleisli (`<=<`).
writeProseMirror _ =
  pure . pmDocToJSONText
    <=< convertToPMDoc
    <=< reportAndDropUnrepresentableContent . reconcileWithProseMirrorModel
  where
    -- Like Pandoc's writers, reports the content the format can't hold and leaves it out.
    reportAndDropUnrepresentableContent :: (PandocMonad m) => Pandoc.Pandoc -> m Pandoc.Pandoc
    reportAndDropUnrepresentableContent doc = do
      mapM_ (report . BlockNotRendered) (findUnrepresentableBlocks doc)
      mapM_ (report . InlineNotRendered) (findUnrepresentableInlines doc)
      pure (dropUnrepresentableContent doc)

    -- The doc's metadata is the document's, not the tree's.
    convertToPMDoc :: (PandocMonad m) => Pandoc.Pandoc -> m PM.PMDoc
    convertToPMDoc doc@(Pandoc.Pandoc meta _) = fmap addMeta (convertTreeToPMDoc (buildDocTree doc))
      where
        addMeta :: PM.PMDoc -> PM.PMDoc
        addMeta (PM.PMDoc root) = PM.PMDoc (addDocMeta meta root)

    buildDocTree :: Pandoc.Pandoc -> Tree GroupedInlinesTree.DocNode
    buildDocTree = withoutMetadataSubtree . unwrapFigureContentParaOrPlain . GroupedInlinesTree.toTree

    convertTreeToPMDoc :: (PandocMonad m) => Tree GroupedInlinesTree.DocNode -> m PM.PMDoc
    convertTreeToPMDoc = either (throwError . toPandocError) pure . (pmDocFromPMTree <=< groupedInlinesPandocTreeToPMTree)

    toPandocError :: PMConversionError -> PandocError
    toPandocError (NodeSpecViolationError message) = PandocSomeError (T.pack message)
    toPandocError (UnexpectedTreeShapeError message) = PandocShouldNeverHappenError (T.pack message)
    toPandocError (InvariantError message) = PandocShouldNeverHappenError (T.pack message)

pmDocToJSONText :: PM.PMDoc -> T.Text
pmDocToJSONText = decodeUtf8 . BSL8.toStrict . encode
