module ProseMirror.Utils.Json (parseNonEmpty) where

import Data.Aeson.Types (Parser)
import qualified Data.Text as T

parseNonEmpty :: String -> T.Text -> Parser T.Text
parseNonEmpty fieldName txt
  | T.null txt = fail $ fieldName ++ " must be non-empty"
  | otherwise = pure txt
