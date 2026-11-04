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
import ProseMirror.Metadata (addDocFrontMatter, writeDocFrontMatter)
import qualified ProseMirror.Model as PM (FrontMatter, PMDoc (..))
import ProseMirror.PandocTreeShape (dropUnrepresentableContent, findUnrepresentableBlocks, findUnrepresentableInlines, reconcileWithProseMirrorModel)
import ProseMirror.PandocTreeShape.FigureContent.GroupedInlines (unwrapFigureContentParaOrPlain)
import ProseMirror.PandocTreeShape.Metadata.GroupedInlines (withoutMetadataSubtree)
import ProseMirror.Tree (groupedInlinesPandocTreeToPMTree, pmDocFromPMTree)
import Text.Pandoc (PandocError (PandocShouldNeverHappenError, PandocSomeError), WriterOptions)
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Definition as Pandoc (Pandoc (..))
import Text.Pandoc.Logging (LogMessage (BlockNotRendered, InlineNotRendered))

writeProseMirror :: (PandocMonad m) => WriterOptions -> Pandoc.Pandoc -> m T.Text
writeProseMirror opts document = do
  -- The doc's metadata is the document's, not the tree's.
  frontMatter <- writeDocFrontMatter opts document
  -- Some steps yield their result inside the Pandoc monad, so we compose with Kleisli (`<=<`).
  pmDoc <- (convertToPMDoc <=< reportAndDropUnrepresentableContent . reconcileWithProseMirrorModel) document
  pure (pmDocToJSONText (addFrontMatter frontMatter pmDoc))
  where
    -- Like Pandoc's writers, reports the content the format can't hold and leaves it out.
    reportAndDropUnrepresentableContent :: (PandocMonad m) => Pandoc.Pandoc -> m Pandoc.Pandoc
    reportAndDropUnrepresentableContent doc = do
      mapM_ (report . BlockNotRendered) (findUnrepresentableBlocks doc)
      mapM_ (report . InlineNotRendered) (findUnrepresentableInlines doc)
      pure (dropUnrepresentableContent doc)

    convertToPMDoc :: (PandocMonad m) => Pandoc.Pandoc -> m PM.PMDoc
    convertToPMDoc = convertTreeToPMDoc . buildDocTree

    addFrontMatter :: Maybe PM.FrontMatter -> PM.PMDoc -> PM.PMDoc
    addFrontMatter frontMatter (PM.PMDoc root) = PM.PMDoc (addDocFrontMatter frontMatter root)

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
