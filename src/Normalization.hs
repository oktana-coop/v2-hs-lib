module Normalization (normalizeFrontMatterForProseMirror) where

import Control.Monad ((>=>))
import Conversion (pandocReaderOptions, pandocWriterOptions, toReadError, toUnrepresentableContentError, toWriteError)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Text as T
import Error (Error (InvalidMetadataError))
import Format (Format (Markdown, ProseMirror))
import Markdown.FrontMatter (isClosingDelimiter)
import Markdown.PandocReader (readFrontMatter)
import ProseMirror.Metadata (writeDocFrontMatter)
import ProseMirror.PandocTreeShape (UnrepresentableContent, findUnrepresentableContent, reconcileWithProseMirrorModel)
import Text.Pandoc (Meta, Pandoc (..), runPure)

normalizeFrontMatterForProseMirror :: T.Text -> Either (NonEmpty Error) (Maybe T.Text)
normalizeFrontMatterForProseMirror = first pure . (checkNoClosingDelimiter >=> readMetadata >=> checkRepresentable >=> writeForProseMirror)
  where
    -- Such a line would end the front matter, and what follows it would be read as something else.
    checkNoClosingDelimiter :: T.Text -> Either Error T.Text
    checkNoClosingDelimiter yaml
      | any isClosingDelimiter (T.lines yaml) = Left (InvalidMetadataError (T.pack "A line with only --- or ... would end the front matter."))
      | otherwise = Right yaml

    readMetadata :: T.Text -> Either Error Meta
    readMetadata = first (toReadError Markdown) . runPure . readFrontMatter pandocReaderOptions

    checkRepresentable :: Meta -> Either Error Meta
    checkRepresentable meta = case nonEmpty (unrepresentableContent meta) of
      Nothing -> Right meta
      Just content -> Left (toUnrepresentableContentError content)

    writeForProseMirror :: Meta -> Either Error (Maybe T.Text)
    writeForProseMirror = first (toWriteError ProseMirror) . runPure . writeDocFrontMatter pandocWriterOptions . pandocWithMeta

    unrepresentableContent :: Meta -> [UnrepresentableContent]
    unrepresentableContent = findUnrepresentableContent . reconcileWithProseMirrorModel . pandocWithMeta

    pandocWithMeta :: Meta -> Pandoc
    pandocWithMeta meta = Pandoc meta []
