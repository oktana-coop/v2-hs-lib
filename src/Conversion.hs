module Conversion (convertFromAutomerge, convertToAutomerge, convertToBinary, convertToText, readFrom, pandocReaderOptions, pandocWriterOptions, Format (..)) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (throwError)
import Control.Monad.Trans (MonadIO (..))
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Format (Format (..))
import PandocReader as AutomergePandoc.PandocReader (readAutomerge)
import PandocWriter as AutomergePandoc.PandocWriter (writeAutomerge)
import ProseMirror.PandocReader (readProseMirror)
import ProseMirror.PandocWriter (writeProseMirror)
import Text.Pandoc (Pandoc, PandocError (PandocSomeError), PandocIO, PandocMonad, ReaderOptions, WrapOption (WrapPreserve), WriterOptions (writerWrapText), def, readHtml, readJSON, readMarkdown, readNative, readerExtensions, writerExtensions)
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Extensions (Extension (Ext_fenced_code_blocks, Ext_footnotes), enableExtension, pandocExtensions)
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.Writers (writeDocx, writeHtml5String, writeJSON, writeLaTeX, writeMarkdown, writeNative)

writeToBytes :: (PandocMonad m, MonadIO m, MonadMask m) => Format -> WriterOptions -> Pandoc -> m BL.ByteString
writeToBytes format = case format of
  Pandoc -> encodeTextWriter writeNative
  Markdown -> encodeTextWriter writeMarkdown
  Html -> encodeTextWriter writeHtml5String
  Json -> encodeTextWriter writeJSON
  Automerge -> encodeTextWriter writeAutomerge
  ProseMirror -> encodeTextWriter writeProseMirror
  Pdf -> writePdf
  Docx -> writeDocx

encodeTextWriter :: (PandocMonad m) => (WriterOptions -> Pandoc -> m T.Text) -> WriterOptions -> Pandoc -> m BL.ByteString
encodeTextWriter writer opts doc = fmap (BL.fromStrict . TE.encodeUtf8) (writer opts doc)

-- This seems to need some more work because we get an error when trying to convert with `pdflatex` (the default).
-- TODO: Explore emulating the --standalone argument in code, or passing a default Latex template.
-- Alternatively, we don't necessarily need Latex, we can use another engine.
writePdf :: (PandocMonad m, MonadIO m, MonadMask m) => WriterOptions -> Pandoc -> m BL.ByteString
writePdf opts doc = do
  bytes <- makePDF "pdflatex" [] writeLaTeX opts doc
  -- Convert the binary error to a Pandoc error. In the successful case, just wrap the bytes in the Pandoc monad with `return`.
  either (throwError . toPandocError) return bytes
  where
    toPandocError :: BL.ByteString -> PandocError
    toPandocError = PandocSomeError . TE.decodeUtf8 . BL.toStrict

readFrom :: Format -> ReaderOptions -> T.Text -> PandocIO Pandoc
readFrom format = case format of
  Pandoc -> readNative
  Markdown -> readMarkdown
  Html -> readHtml
  Json -> readJSON
  Automerge -> readAutomerge
  ProseMirror -> readProseMirror
  -- TODO: Implement these. For the ones that cannot implemented (potentially PDF), return an error
  Pdf -> undefined
  Docx -> undefined

-- Explicitly enabling the fenced code blocks and footnotes extensions. For some reason they aren't included when
-- we're just using `def` in the reader/writer options.
-- TODO: Investigate why, in theory `def` includes fenced code blocks too,
-- so we must understand why it needs this special treatment.
pandocReaderOptions :: ReaderOptions
pandocReaderOptions = def {readerExtensions = enableExtension Ext_footnotes $ enableExtension Ext_fenced_code_blocks pandocExtensions}

pandocWriterOptions :: WriterOptions
pandocWriterOptions = def {writerWrapText = WrapPreserve, writerExtensions = enableExtension Ext_footnotes $ enableExtension Ext_fenced_code_blocks pandocExtensions}

convert :: Format -> Format -> String -> IO (Either PandocError BL.ByteString)
convert inputFormat outputFormat input = do
  runIO $ do
    doc <- readFrom inputFormat pandocReaderOptions (T.pack input)
    writeToBytes outputFormat pandocWriterOptions doc

convertToText :: Format -> Format -> String -> IO (Either (NonEmpty PandocError) T.Text)
convertToText inputFormat outputFormat input = (fmap . fmap) byteStringToText (wrapErrorToNonEmptyList $ convert inputFormat outputFormat input)
  where
    wrapErrorToNonEmptyList :: IO (Either PandocError BL.ByteString) -> IO (Either (NonEmpty PandocError) BL.ByteString)
    -- `first` acts on the first argument of a BiFunctor like Either (the Left value in our case).
    -- `pure` wraps the error in the NonEmpty list structure.
    wrapErrorToNonEmptyList = fmap (first pure)

    byteStringToText :: BL.ByteString -> T.Text
    byteStringToText = TE.decodeUtf8 . BL.toStrict

convertToBinary :: Format -> Format -> String -> IO BL.ByteString
convertToBinary inputFormat outputFormat input = do
  result <- convert inputFormat outputFormat input
  successBytes <- handleError result
  return successBytes

convertFromAutomerge :: Format -> String -> IO (Either (NonEmpty PandocError) T.Text)
convertFromAutomerge outputFormat = convertToText Automerge outputFormat

convertToAutomerge :: Format -> String -> IO (Either (NonEmpty PandocError) T.Text)
convertToAutomerge inputFormat = convertToText inputFormat Automerge