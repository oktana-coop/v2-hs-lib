module Main (main) where

import Cli (Command (..), readInputCommand)
import Conversion (convertFromAutomerge, convertToAutomerge, convertToBinary, convertToText)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List.NonEmpty (NonEmpty, toList)
import Diff (proseMirrorDiff)
import Response (ErrorOutput (..), Response (..))
import Text.Pandoc (PandocError)
import Text.Pandoc.Error (renderError)

wrapToResponse :: Either (NonEmpty PandocError) a -> Response a
wrapToResponse result = case result of
  Left errors -> Failure $ map (ErrorMessage . renderError) (toList errors)
  Right value -> Success value

wrapToReponseAndPrint :: (ToJSON a) => (Either (NonEmpty PandocError) a) -> IO ()
wrapToReponseAndPrint = BL.putStrLn . encode . wrapToResponse

main :: IO ()
main = do
  command <- readInputCommand
  case command of
    ConvertFromAutomerge format str -> convertFromAutomerge format str >>= wrapToReponseAndPrint
    ConvertToAutomerge format str -> convertToAutomerge format str >>= wrapToReponseAndPrint
    ConvertToText from to str -> convertToText from to str >>= wrapToReponseAndPrint
    ConvertToBinary from to str -> convertToBinary from to str >>= BL.putStr
    ProseMirrorDiff format str1 str2 -> proseMirrorDiff format str1 str2 >>= wrapToReponseAndPrint
