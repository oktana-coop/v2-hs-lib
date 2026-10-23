{-# LANGUAGE OverloadedStrings #-}

module Response (Response (..), ErrorOutput, errorOutput) where

import Data.Aeson (ToJSON, object, (.=))
import Data.Aeson.Types (ToJSON (..))
import qualified Data.Text as T
import Error (Error (..))

data ErrorOutput = ErrorOutput T.Text T.Text deriving (Show)

errorOutput :: Error -> ErrorOutput
errorOutput (ReadError _ message) = ErrorOutput "readError" message
errorOutput (WriteError _ message) = ErrorOutput "writeError" message
errorOutput (PatchError message) = ErrorOutput "patchError" message

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
  toJSON (ErrorOutput errorType message) =
    object ["type" .= errorType, "message" .= message]
