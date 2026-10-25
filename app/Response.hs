{-# LANGUAGE OverloadedStrings #-}

module Response (Response (..), ErrorOutput, errorOutput) where

import Data.Aeson (ToJSON, object, (.=))
import Data.Aeson.Types (Pair, ToJSON (..))
import Data.List.NonEmpty (toList)
import qualified Data.Text as T
import Error (Error (..), UnrepresentableContent (..))

-- The error's type, its message, and the fields of its type.
data ErrorOutput = ErrorOutput T.Text T.Text [Pair] deriving (Show)

errorOutput :: Error -> ErrorOutput
errorOutput (ReadError _ message) = ErrorOutput "readError" message []
errorOutput (InvalidMetadataError message) = ErrorOutput "invalidMetadataError" message []
errorOutput (WriteError _ message) = ErrorOutput "writeError" message []
errorOutput (UnrepresentableContentError content) =
  ErrorOutput "unrepresentableContentError" ("The document has content ProseMirror can't represent: " <> T.intercalate ", " names) ["content" .= names]
  where
    names = map contentName (toList content)
errorOutput (PatchError message) = ErrorOutput "patchError" message []

contentName :: UnrepresentableContent -> T.Text
contentName TableContent = "tables"
contentName DefinitionListContent = "definitionLists"
contentName LineBlockContent = "lineBlocks"
contentName RawBlockContent = "rawBlocks"

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
  toJSON (ErrorOutput errorType message fields) =
    object (["type" .= errorType, "message" .= message] ++ fields)
