module ProseMirror.PandocWriter (writeProseMirror) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified DocTree.GroupedInlines (toTree)
import qualified ProseMirror.PMJson as PM (PMDoc (..))
import ProseMirror.PMTree (groupedInlinesPandocTreeToPMTree, pmDocFromPMTree)
import Text.Pandoc (WriterOptions)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition as Pandoc (Pandoc)

toJSONText :: PM.PMDoc -> T.Text
toJSONText = decodeUtf8 . BSL8.toStrict . encode

writeProseMirror :: (PandocMonad m) => WriterOptions -> Pandoc.Pandoc -> m T.Text
writeProseMirror _ = pure . toJSONText . pmDocFromPMTree . groupedInlinesPandocTreeToPMTree . DocTree.GroupedInlines.toTree
