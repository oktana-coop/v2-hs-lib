{-# LANGUAGE OverloadedStrings #-}

module ProseMirror.Model.Mark (Mark (..), Link (..)) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.:?), (.=))
import qualified Data.Text as T
import ProseMirror.Utils.Json (parseNonEmpty)

-- TODO: Make linkTitle optional
data Link = Link {url :: T.Text, linkTitle :: T.Text} deriving (Show, Eq)

instance FromJSON Link where
  parseJSON = withObject "Link" $ \v -> do
    linkUrl <- v .: "href" >>= parseNonEmpty "href"
    title <- v .: "title"
    pure Link {url = linkUrl, linkTitle = title}

instance ToJSON Link where
  toJSON link = object ["href" .= url link, "title" .= linkTitle link]

data Mark
  = Strong
  | Emphasis
  | LinkMark Link
  | Code
  deriving (Show, Eq)

instance FromJSON Mark where
  parseJSON = withObject "Mark" $ \v -> do
    mType <- v .: "type" >>= parseNonEmpty "type"
    mAttrs <- v .:? "attrs"
    case mType of
      "strong" -> pure Strong
      "em" -> pure Emphasis
      "link" -> case mAttrs of
        Just attrs -> fmap LinkMark $ parseJSON attrs
        Nothing -> fail "Missing attrs for link mark"
      "code" -> pure Code
      _ -> fail "Invalid mark"

instance ToJSON Mark where
  toJSON Strong = object ["type" .= T.pack "strong"]
  toJSON Emphasis = object ["type" .= T.pack "em"]
  toJSON (LinkMark link) = object ["type" .= T.pack "link", "attrs" .= toJSON link]
  toJSON Code = object ["type" .= T.pack "code"]
