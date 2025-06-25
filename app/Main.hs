{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cli (Command (..), Format (..), readInputCommand)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace (trace)
import PandocReader as AutomergePandoc.PandocReader (readAutomerge)
import PandocWriter as AutomergePandoc.PandocWriter (writeAutomerge)
import ProseMirror.Diff (DecoratedPMDoc, toDecoratedPMDoc)
import ProseMirror.PandocWriter (writeProseMirror)
import Response (ErrorOutput (..), Response (..))
import RichTextDiff (getAnnotatedTree)
import Text.Pandoc (Pandoc, PandocIO, PandocMonad, ReaderOptions, WriterOptions, def, readHtml, readJSON, readMarkdown, readNative, readerExtensions)
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Error (PandocError, handleError, renderError)
import Text.Pandoc.Extensions (Extension (Ext_fenced_code_blocks), enableExtension, pandocExtensions)
import Text.Pandoc.Writers (writeHtml5String, writeJSON, writeMarkdown, writeNative)

writeTo :: (PandocMonad m) => Format -> WriterOptions -> Pandoc -> m T.Text
writeTo format = case format of
  Cli.Pandoc -> writeNative
  Cli.Markdown -> writeMarkdown
  Cli.Html -> writeHtml5String
  Cli.Json -> writeJSON
  Cli.Automerge -> writeAutomerge
  Cli.ProseMirror -> writeProseMirror

readFrom :: Format -> ReaderOptions -> T.Text -> PandocIO Pandoc
readFrom format = case format of
  Cli.Pandoc -> readNative
  Cli.Markdown -> readMarkdown
  Cli.Html -> readHtml
  Cli.Json -> readJSON
  Cli.Automerge -> readAutomerge
  Cli.ProseMirror -> undefined

convert :: Format -> Format -> String -> IO ()
convert inputFormat outputFormat input = do
  result <- runIO $ do
    doc <- readFrom inputFormat readerOpts (T.pack input)
    writeTo outputFormat def doc
  rst <- handleError result
  TIO.putStrLn rst
  where
    -- Explicitly enabling the fenced code blocks extension. For some reason it wasn't included when
    -- we were just using `def` in the reader options.
    -- TODO: Investigate why, in theory `def` includes fenced code blocks too,
    -- so we must understand why it needs this special treatment.
    readerOpts = def {readerExtensions = enableExtension Ext_fenced_code_blocks pandocExtensions}

convertFromAutomerge :: Format -> String -> IO ()
convertFromAutomerge outputFormat = convert Automerge outputFormat

convertToAutomerge :: Format -> String -> IO ()
convertToAutomerge inputFormat = convert inputFormat Automerge

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
    Convert from to str -> convert from to str
    ProseMirrorDiff format str1 str2 -> produceProseMirrorDiff format str1 str2
