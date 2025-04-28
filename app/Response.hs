{-# LANGUAGE OverloadedStrings #-}

module Response (Response (..), ErrorOutput (..)) where

import Data.Aeson (ToJSON, object, (.=))
import Data.Aeson.Types (ToJSON (..))
import qualified Data.Text as T

data ErrorOutput = ErrorMessage T.Text deriving (Show)

data Response a
  = Success a
  | Failure [ErrorOutput]
  deriving (Show)

instance (ToJSON a) => ToJSON (Response a) where
  toJSON (Success d) =
    object ["data" .= d]
  toJSON (Failure e) =
    object ["errors" .= e]

instance ToJSON ErrorOutput where
  toJSON (ErrorMessage message) =
    object ["message" .= message]