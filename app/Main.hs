{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cli (Command (..), Format (..), readInputCommand)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (throwError)
import Control.Monad.Trans (MonadIO (..))
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Debug.Trace (trace)
import PandocReader as AutomergePandoc.PandocReader (readAutomerge)
import PandocWriter as AutomergePandoc.PandocWriter (writeAutomerge)
import ProseMirror.Diff (DecoratedPMDoc, toDecoratedPMDoc)
import ProseMirror.PandocReader (readProseMirror)
import ProseMirror.PandocWriter (writeProseMirror)
import Response (ErrorOutput (..), Response (..))
import RichTextDiff (getAnnotatedTree)
import Text.Pandoc (Pandoc, PandocError (PandocSomeError), PandocIO, PandocMonad, ReaderOptions, WriterOptions, def, readHtml, readJSON, readMarkdown, readNative, readerExtensions, writerExtensions)
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Error (handleError, renderError)
import Text.Pandoc.Extensions (Extension (Ext_fenced_code_blocks, Ext_footnotes), enableExtension, pandocExtensions)
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.Writers (writeDocx, writeHtml5String, writeJSON, writeLaTeX, writeMarkdown, writeNative)

writeToBytes :: (PandocMonad m, MonadIO m, MonadMask m) => Format -> WriterOptions -> Pandoc -> m BL.ByteString
writeToBytes format = case format of
  Cli.Pandoc -> encodeTextWriter writeNative
  Cli.Markdown -> encodeTextWriter writeMarkdown
  Cli.Html -> encodeTextWriter writeHtml5String
  Cli.Json -> encodeTextWriter writeJSON
  Cli.Automerge -> encodeTextWriter writeAutomerge
  Cli.ProseMirror -> encodeTextWriter writeProseMirror
  Cli.Pdf -> writePdf
  Cli.Docx -> writeDocx

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
  Cli.Pandoc -> readNative
  Cli.Markdown -> readMarkdown
  Cli.Html -> readHtml
  Cli.Json -> readJSON
  Cli.Automerge -> readAutomerge
  -- TODO: Implement these. For the ones that cannot implemented (potentially PDF), return an error
  Cli.ProseMirror -> readProseMirror
  Cli.Pdf -> undefined
  Cli.Docx -> undefined

convert :: Format -> Format -> String -> IO (Either PandocError BL.ByteString)
convert inputFormat outputFormat input = do
  runIO $ do
    doc <- readFrom inputFormat readerOpts (T.pack input)
    writeToBytes outputFormat writerOpts doc
  where
    -- Explicitly enabling the fenced code blocks and footnotes extensions. For some reason they aren't included when
    -- we're just using `def` in the reader/writer options.
    -- TODO: Investigate why, in theory `def` includes fenced code blocks too,
    -- so we must understand why it needs this special treatment.
    readerOpts = def {readerExtensions = enableExtension Ext_footnotes $ enableExtension Ext_fenced_code_blocks pandocExtensions}
    writerOpts = def {writerExtensions = enableExtension Ext_footnotes $ enableExtension Ext_fenced_code_blocks pandocExtensions}

convertToText :: Format -> Format -> String -> IO ()
convertToText inputFormat outputFormat input = do
  result <- convert inputFormat outputFormat input
  successBytes <- handleError result
  (TIO.putStrLn . TE.decodeUtf8 . BL.toStrict) successBytes

convertToBinary :: Format -> Format -> String -> IO ()
convertToBinary inputFormat outputFormat input = do
  result <- convert inputFormat outputFormat input
  successBytes <- handleError result
  BL.putStr successBytes

convertFromAutomerge :: Format -> String -> IO ()
convertFromAutomerge outputFormat = convertToText Automerge outputFormat

convertToAutomerge :: Format -> String -> IO ()
convertToAutomerge inputFormat = convertToText inputFormat Automerge

wrapToResponse :: Either PandocError a -> Response a
wrapToResponse result = case result of
  Left err -> Failure [ErrorMessage $ renderError err]
  Right value -> Success value

responseForErrorsList :: [PandocError] -> Response a
responseForErrorsList pandocErrors = Failure $ map (ErrorMessage . renderError) pandocErrors

produceProseMirrorDiff :: Format -> String -> String -> IO ()
produceProseMirrorDiff format doc1Str doc2Str = do
  eitherDoc1 <- runIO $ readFrom format def (T.pack $ trace doc1Str $ doc1Str)
  eitherDoc2 <- runIO $ readFrom format def (T.pack $ trace doc2Str $ doc2Str)

  case (eitherDoc1, eitherDoc2) of
    (Right doc1, Right doc2) -> BL.putStrLn $ encode $ wrapToResponse $ pure $ toDecoratedPMDoc $ getAnnotatedTree doc1 doc2
    (failedDoc@(Left _), Right _) -> BL.putStrLn $ encode $ wrapToResponse failedDoc
    (Right _, failedDoc@(Left _)) -> BL.putStrLn $ encode $ wrapToResponse failedDoc
    ((Left err1), (Left err2)) -> BL.putStrLn $ encode $ (responseForErrorsList [err1, err2] :: Response DecoratedPMDoc)

main :: IO ()
main = do
  command <- readInputCommand
  case command of
    ConvertFromAutomerge format str -> convertFromAutomerge format str
    ConvertToAutomerge format str -> convertToAutomerge format str
    ConvertToText from to str -> convertToText from to str
    ConvertToBinary from to str -> convertToBinary from to str
    ProseMirrorDiff format str1 str2 -> produceProseMirrorDiff format str1 str2
