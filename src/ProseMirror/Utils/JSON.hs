module ProseMirror.Utils.Json (parseNonEmpty, toJsonText, fromJsonText) where

import Data.Aeson (FromJSON, eitherDecodeStrict)
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (Parser, ToJSON)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as LT

parseNonEmpty :: String -> T.Text -> Parser T.Text
parseNonEmpty fieldName txt
  | T.null txt = fail $ fieldName ++ " must be non-empty"
  | otherwise = pure txt

toJsonText :: (ToJSON a) => a -> T.Text
toJsonText = LT.toStrict . encodeToLazyText

fromJsonText :: (FromJSON a) => T.Text -> Either String a
fromJsonText = eitherDecodeStrict . encodeUtf8