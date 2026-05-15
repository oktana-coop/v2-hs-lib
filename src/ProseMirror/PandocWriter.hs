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
import ProseMirror.PandocTreeShape.ImplicitFigure (stripCaptionEqualToAlt)
import ProseMirror.Tree (groupedInlinesPandocTreeToPMTree, pmDocFromPMTree)
import Text.Pandoc (PandocError (PandocParseError), WriterOptions)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition as Pandoc (Pandoc)

writeProseMirror :: (PandocMonad m) => WriterOptions -> Pandoc.Pandoc -> m T.Text
-- Note that convertTreeToPMDocAndHandleErrors wraps the PM doc in the Pandoc monad.
-- Therefore, to compose we need Kleisli composition instead of the normal composition operator.
writeProseMirror _ = pure . pmDocToJSONText <=< convertTreeToPMDocAndHandleErrors . unwrapFigureContentParaOrPlain . GroupedInlinesTree.toTree . stripCaptionEqualToAlt
  where
    -- Chains the two `Either String`-returning conversions (tree → PMTree → PMDoc) before wrapping in the Pandoc monad.
    convertTreeToPMDocAndHandleErrors :: (PandocMonad m) => Tree GroupedInlinesTree.DocNode -> m PM.PMDoc
    convertTreeToPMDocAndHandleErrors = either handlePMConversionErrorMessage pure . (pmDocFromPMTree <=< groupedInlinesPandocTreeToPMTree)

    handlePMConversionErrorMessage :: (PandocMonad m) => String -> m a
    handlePMConversionErrorMessage = throwError . PandocParseError . T.pack

pmDocToJSONText :: PM.PMDoc -> T.Text
pmDocToJSONText = decodeUtf8 . BSL8.toStrict . encode
