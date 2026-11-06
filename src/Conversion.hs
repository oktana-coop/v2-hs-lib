module Conversion (convertFromAutomerge, convertToAutomerge, convertToBinary, convertToText, readFrom, readDocument, readDocuments, toReadError, toUnrepresentableContentError, toWriteError, pandocReaderOptions, pandocWriterOptions, Format (..)) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (throwError)
import Control.Monad.Trans (MonadIO (..))
import Data.Bifunctor (bimap, first)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List.NonEmpty (NonEmpty, nonEmpty, nub)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Error (Error (..))
import Format (Format (..))
import Markdown.FrontMatter (invalidFrontMatter)
import Markdown.PandocReader (readFrontMatter, readMarkdown)
import Markdown.PandocWriter (writeNormalizedMarkdown)
import Metadata.KeyOutline (stripTempMetaOutlineKey)
import PandocReader as AutomergePandoc.PandocReader (readAutomerge)
import PandocWriter as AutomergePandoc.PandocWriter (writeAutomerge)
import ProseMirror.PandocReader (readProseMirror)
import ProseMirror.PandocTreeShape (UnrepresentableContent, findUnrepresentableMetadataContent, maybeUnrepresentableBlockContent, maybeUnrepresentableInlineContent)
import ProseMirror.PandocWriter (writeProseMirror)
import Text.Pandoc (Pandoc, PandocError (PandocShouldNeverHappenError, PandocSomeError, PandocUnknownReaderError), PandocIO, PandocMonad, ReaderOptions, WrapOption (WrapPreserve), WriterOptions (writerWrapText), def, readHtml, readJSON, readNative, readerExtensions, writerExtensions, writerTemplate)
import Text.Pandoc.Class (runIO, runPure, setResourcePath)
import qualified Text.Pandoc.Definition as PandocDef
import Text.Pandoc.Error (renderError)
import Text.Pandoc.Extensions (Extension (Ext_fenced_code_blocks, Ext_footnotes, Ext_yaml_metadata_block), enableExtension, pandocExtensions)
import Text.Pandoc.Logging (LogMessage (BlockNotRendered, InlineNotRendered))
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.SelfContained (makeSelfContained)
import Text.Pandoc.Templates (compileDefaultTemplate)
import Text.Pandoc.Writers (writeDocx, writeHtml5String, writeJSON, writeLaTeX, writeNative)
import Utils.Either (bothOrErrors)
import Utils.Pandoc (isYamlMetadataError, withLog)

writeToBytes :: (PandocMonad m, MonadIO m, MonadMask m) => Bool -> Format -> WriterOptions -> Pandoc -> m BL.ByteString
writeToBytes embedResources format opts = writer opts . (if keepsMetaOrder format then id else stripFromDocument)
  where
    writer = case format of
      Pandoc -> encodeTextWriter writeNative
      Markdown -> encodeTextWriter writeNormalizedMarkdown
      Html -> encodeTextWriter (writeHtml embedResources)
      Json -> encodeTextWriter writeJSON
      Automerge -> encodeTextWriter writeAutomerge
      ProseMirror -> encodeTextWriter writeProseMirror
      Pdf -> writePdf
      Docx -> writeDocx

    stripFromDocument :: Pandoc -> Pandoc
    stripFromDocument (PandocDef.Pandoc meta blocks) = PandocDef.Pandoc (stripTempMetaOutlineKey meta) blocks

-- The writers that read the key outline from the temporary metadata key.
keepsMetaOrder :: Format -> Bool
keepsMetaOrder Markdown = True
keepsMetaOrder ProseMirror = True
keepsMetaOrder _ = False

-- When embedding is requested, inline referenced local resources (e.g. images)
-- resolved via the resource path as data URIs, producing self-contained HTML.
writeHtml :: (PandocMonad m) => Bool -> WriterOptions -> Pandoc -> m T.Text
writeHtml embedResources opts doc = do
  html <- writeHtml5String opts doc
  if embedResources then makeSelfContained html else pure html

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
  -- TODO: Implement DOCX. PDF can't be read.
  Pdf -> noReader
  Docx -> noReader
  where
    noReader :: ReaderOptions -> T.Text -> PandocIO Pandoc
    noReader _ _ = throwError . PandocUnknownReaderError . T.toLower . T.pack . show $ format

readDocument :: Format -> T.Text -> IO (Either Error Pandoc)
readDocument format input = do
  result <- runIO (readFrom format pandocReaderOptions input)
  pure (first (toReadError format) result)

toReadError :: Format -> PandocError -> Error
toReadError format pandocError
  | isYamlMetadataError pandocError = InvalidMetadataError pandocErrorMessage
  | otherwise = ReadError format pandocErrorMessage
  where
    pandocErrorMessage :: T.Text
    pandocErrorMessage = renderError pandocError

-- Explicitly enabling the fenced code blocks and footnotes extensions. For some reason they aren't included when
-- we're just using `def` in the reader/writer options.
-- TODO: Investigate why, in theory `def` includes fenced code blocks too,
-- so we must understand why it needs this special treatment.
pandocReaderOptions :: ReaderOptions
pandocReaderOptions = def {readerExtensions = enableExtension Ext_footnotes $ enableExtension Ext_fenced_code_blocks $ enableExtension Ext_yaml_metadata_block pandocExtensions}

pandocWriterOptions :: WriterOptions
pandocWriterOptions = def {writerWrapText = WrapPreserve, writerExtensions = enableExtension Ext_footnotes $ enableExtension Ext_fenced_code_blocks $ enableExtension Ext_yaml_metadata_block pandocExtensions}

writeTo :: Format -> Maybe FilePath -> Pandoc -> PandocIO BL.ByteString
writeTo outputFormat maybeResourcePath doc = do
  -- Tell Pandoc where to resolve referenced assets from when it embeds them (HTML/Docx).
  mapM_ (\dir -> setResourcePath [dir]) maybeResourcePath
  opts <- addTemplate outputFormat pandocWriterOptions
  writeToBytes (shouldEmbedResources outputFormat maybeResourcePath) outputFormat opts doc
  where
    shouldEmbedResources :: Format -> Maybe FilePath -> Bool
    shouldEmbedResources Html (Just _) = True
    shouldEmbedResources _ _ = False

-- Without the template metadata is not included in the output document, even if the extension is present.
addTemplate :: (PandocMonad m) => Format -> WriterOptions -> m WriterOptions
addTemplate format opts = case format of
  Markdown -> setTemplate (T.pack "markdown") opts
  _ -> return opts
  where
    setTemplate :: (PandocMonad m) => T.Text -> WriterOptions -> m WriterOptions
    setTemplate fmt options = do
      tmpl <- compileDefaultTemplate fmt
      return options {writerTemplate = Just tmpl}

writeDocument :: Format -> Maybe FilePath -> Pandoc -> IO (Either Error BL.ByteString)
writeDocument format maybeResourcePath doc@(PandocDef.Pandoc meta _) = case invalidFrontMatter meta of
  Just yaml | shouldFailOnInvalidFrontMatter -> pure (Left (invalidFrontMatterError yaml))
  _ -> do
    result <- runIO (withLog (writeTo format maybeResourcePath doc))
    pure (first (toWriteError format) result >>= checkForUnrepresentableContent)
  where
    -- Markdown and ProseMirror are lenient, keeping the raw text, so that front matter Pandoc can't read can still
    -- be fixed. The other formats fail on it.
    shouldFailOnInvalidFrontMatter :: Bool
    shouldFailOnInvalidFrontMatter = format `notElem` [Markdown, ProseMirror, Automerge]

    -- The metadata keeps only the raw text, not Pandoc's error for it, so it's read again to get that error back.
    invalidFrontMatterError :: T.Text -> Error
    invalidFrontMatterError yaml = case runPure (readFrontMatter pandocReaderOptions yaml) of
      Left readError -> toReadError Markdown readError
      Right _ -> InvalidMetadataError (T.pack "The front matter isn't valid YAML.")

    -- Pandoc's writers leave out the blocks their format can't hold, and only report them in the log.
    -- The ProseMirror writer does the same, and here we produce an error in the case of unrepresentable 
    -- content.
    checkForUnrepresentableContent :: (BL.ByteString, [LogMessage]) -> Either Error BL.ByteString
    checkForUnrepresentableContent (bytes, messages) = case (format, nonEmpty (findUnrepresentableContentInLog messages <> findUnrepresentableMetadataContent meta)) of
      (ProseMirror, Just unrepresentableContent) -> Left (toUnrepresentableContentError unrepresentableContent)
      _ -> Right bytes

toWriteError :: Format -> PandocError -> Error
toWriteError format = WriteError format . writeErrorMessage format
  where
    writeErrorMessage :: Format -> PandocError -> T.Text
    -- The ProseMirror writer is the library's own, so its broken invariants are the library's bugs.
    writeErrorMessage ProseMirror (PandocShouldNeverHappenError message) = message
    writeErrorMessage _ pandocError = renderError pandocError

findUnrepresentableContentInLog :: [LogMessage] -> [UnrepresentableContent]
findUnrepresentableContentInLog messages =
  mapMaybe maybeUnrepresentableBlockContent [block | BlockNotRendered block <- messages]
    <> mapMaybe maybeUnrepresentableInlineContent [inline | InlineNotRendered inline <- messages]

toUnrepresentableContentError :: NonEmpty UnrepresentableContent -> Error
toUnrepresentableContentError = UnrepresentableContentError . nub

convert :: Format -> Format -> Maybe FilePath -> String -> IO (Either Error BL.ByteString)
convert inputFormat outputFormat maybeResourcePath input = do
  eitherDoc <- readDocument inputFormat (T.pack input)
  either (pure . Left) (writeDocument outputFormat maybeResourcePath) eitherDoc

convertToText :: Format -> Format -> Maybe FilePath -> String -> IO (Either (NonEmpty Error) T.Text)
convertToText inputFormat outputFormat maybeResourcePath input = do
  result <- convert inputFormat outputFormat maybeResourcePath input
  -- `pure` wraps the error in the NonEmpty list structure.
  pure (bimap pure byteStringToText result)
  where
    byteStringToText :: BL.ByteString -> T.Text
    byteStringToText = TE.decodeUtf8 . BL.toStrict

convertToBinary :: Format -> Format -> Maybe FilePath -> String -> IO (Either (NonEmpty Error) BL.ByteString)
convertToBinary inputFormat outputFormat maybeResourcePath input = do
  result <- convert inputFormat outputFormat maybeResourcePath input
  pure (first pure result)

convertFromAutomerge :: Format -> String -> IO (Either (NonEmpty Error) T.Text)
convertFromAutomerge outputFormat = convertToText Automerge outputFormat Nothing

convertToAutomerge :: Format -> String -> IO (Either (NonEmpty Error) T.Text)
convertToAutomerge inputFormat input = convertToText inputFormat Automerge Nothing input

-- Reads two documents, each in its own format, reporting the errors of both when both fail.
readDocuments :: Format -> Format -> String -> String -> IO (Either (NonEmpty Error) (Pandoc, Pandoc))
readDocuments format1 format2 doc1Str doc2Str = do
  eitherDoc1 <- readDocument format1 (T.pack doc1Str)
  eitherDoc2 <- readDocument format2 (T.pack doc2Str)
  pure $ bothOrErrors eitherDoc1 eitherDoc2
