module Main (main) where

import Cli (Command (..), readInputCommand)
import Conversion (convertFromAutomerge, convertToAutomerge, convertToBinary, convertToText)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List.NonEmpty (NonEmpty, toList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Diffing (proseMirrorDiff)
import Error (Error)
import Patching (proseMirrorSteps)
import Query (extractAssetUrls)
import Response (Response (..), errorOutput)

wrapToResponse :: Either (NonEmpty Error) a -> Response a
wrapToResponse result = case result of
  Left errors -> Failure $ map errorOutput (toList errors)
  Right value -> Success value

wrapToReponseAndPrint :: (ToJSON a) => (Either (NonEmpty Error) a) -> IO ()
wrapToReponseAndPrint = BL.putStrLn . encode . wrapToResponse

encodeBase64 :: BL.ByteString -> T.Text
encodeBase64 = TE.decodeUtf8 . Base64.encode . BL.toStrict

main :: IO ()
main = do
  command <- readInputCommand
  case command of
    ConvertFromAutomerge format str -> convertFromAutomerge format str >>= wrapToReponseAndPrint
    ConvertToAutomerge format str -> convertToAutomerge format str >>= wrapToReponseAndPrint
    ConvertToText from to resourcePath str -> convertToText from to resourcePath str >>= wrapToReponseAndPrint
    ConvertToBinary from to resourcePath str -> convertToBinary from to resourcePath str >>= wrapToReponseAndPrint . fmap encodeBase64
    ProseMirrorDiff format str1 str2 -> proseMirrorDiff format str1 str2 >>= wrapToReponseAndPrint
    ProseMirrorSteps beforeFormat afterFormat before after -> proseMirrorSteps beforeFormat afterFormat before after >>= wrapToReponseAndPrint
    ExtractAssetUrls format str -> extractAssetUrls format str >>= wrapToReponseAndPrint
