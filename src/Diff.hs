module Diff (proseMirrorDiff) where

import Conversion (pandocReaderOptions, readFrom)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import Format (Format (..))
import ProseMirror.Diff (DecoratedPMDoc, toDecoratedPMDoc)
import RichTextDiff (getAnnotatedTree)
import Text.Pandoc (PandocError)
import Text.Pandoc.Class (runIO)

proseMirrorDiff :: Format -> String -> String -> IO (Either (NonEmpty PandocError) DecoratedPMDoc)
proseMirrorDiff format doc1Str doc2Str = do
  eitherDoc1 <- runIO $ readFrom format pandocReaderOptions (T.pack doc1Str)
  eitherDoc2 <- runIO $ readFrom format pandocReaderOptions (T.pack doc2Str)

  case (eitherDoc1, eitherDoc2) of
    (Right doc1, Right doc2) -> pure $ Right $ toDecoratedPMDoc $ getAnnotatedTree doc1 doc2
    ((Left err), Right _) -> pure $ Left (err :| [])
    (Right _, (Left err)) -> pure $ Left (err :| [])
    ((Left err1), (Left err2)) -> pure $ Left (err1 :| [err2])