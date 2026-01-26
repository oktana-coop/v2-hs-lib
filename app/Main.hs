{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cli (Command (..), readInputCommand)
import Conversion (Format (..), convertFromAutomerge, convertToAutomerge, convertToBinary, convertToText, readFrom)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace (trace)
import ProseMirror.Diff (DecoratedPMDoc, toDecoratedPMDoc)
import Response (ErrorOutput (..), Response (..))
import RichTextDiff (getAnnotatedTree)
import Text.Pandoc (PandocError, def)
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Error (renderError)

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
    ConvertFromAutomerge format str -> convertFromAutomerge format str >>= TIO.putStrLn
    ConvertToAutomerge format str -> convertToAutomerge format str >>= TIO.putStrLn
    ConvertToText from to str -> convertToText from to str >>= TIO.putStrLn
    ConvertToBinary from to str -> convertToBinary from to str >>= BL.putStr
    ProseMirrorDiff format str1 str2 -> produceProseMirrorDiff format str1 str2
