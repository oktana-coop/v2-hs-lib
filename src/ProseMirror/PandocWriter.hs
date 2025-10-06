module ProseMirror.PandocWriter (writeProseMirror) where

import Control.Monad ((<=<))
import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified DocTree.GroupedInlines (toTree)
import qualified ProseMirror.Json as PM (PMDoc (..))
import ProseMirror.Tree (PMTree, groupedInlinesPandocTreeToPMTree, pmDocFromPMTree)
import Text.Pandoc (PandocError (PandocParseError), WriterOptions)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition as Pandoc (Pandoc)

writeProseMirror :: (PandocMonad m) => WriterOptions -> Pandoc.Pandoc -> m T.Text
-- Note that convertPMTreeToDocAndHandleErrors wraps the PM doc to the Pandoc monad.
-- Therefore, to compose we need Kleisli composition instead of the normal composition operator.
writeProseMirror _ = pure . pmDocToJSONText <=< convertPMTreeToDocAndHandleErrors . groupedInlinesPandocTreeToPMTree . DocTree.GroupedInlines.toTree
  where
    -- Here we transform the PM tree to a PM doc that can be converted to JSON string but also
    -- convert a potential error to a Pandoc parsing error.
    -- The result is a PM doc wrapped within a Pandoc monad.
    convertPMTreeToDocAndHandleErrors :: (PandocMonad m) => PMTree -> m PM.PMDoc
    convertPMTreeToDocAndHandleErrors = either handlePMConversionErrorMessage pure . pmDocFromPMTree

    handlePMConversionErrorMessage :: (PandocMonad m) => String -> m a
    handlePMConversionErrorMessage = throwError . PandocParseError . T.pack

pmDocToJSONText :: PM.PMDoc -> T.Text
pmDocToJSONText = decodeUtf8 . BSL8.toStrict . encode
