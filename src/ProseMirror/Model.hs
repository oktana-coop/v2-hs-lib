{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module ProseMirror.Model where

import Data.Aeson (FromJSON (parseJSON), Object, ToJSON (toJSON), Value (Bool, Null, String), eitherDecode, eitherDecodeStrictText, encode, object, withObject, withScientific, withText, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import ProseMirror.Utils.Json (parseNonEmpty)

-- TODO: Make title optional
data Link = Link {url :: T.Text, title :: T.Text} deriving (Show, Eq)

instance FromJSON Link where
  parseJSON = withObject "Link" $ \v -> do
    linkUrl <- v .: "href" >>= parseNonEmpty "href"
    linkTitle <- v .: "title"
    pure Link {url = linkUrl, title = linkTitle}

instance ToJSON Link where
  toJSON link = object ["href" .= url link, "title" .= title link]

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

data TextNode = PMText {text :: T.Text, marks :: Maybe (NonEmpty Mark)} deriving (Show, Eq)

instance FromJSON TextNode where
  parseJSON = withObject "TextNode" $ \v -> do
    nText <- v .: "text" >>= parseNonEmpty "text"
    nMarks <- v .:? "marks"
    pure $ PMText {text = nText, marks = nMarks}

instance ToJSON TextNode where
  toJSON textNode = object $ ["type" .= T.pack "text", "text" .= text textNode, "marks" .= marks textNode]

newtype HeadingLevel = HeadingLevel Int deriving (Show, Eq)

instance FromJSON HeadingLevel where
  parseJSON = withScientific "HeadingLevel" $ \n -> do
    let level = floor n
    if level >= 1 && level <= 6
      then pure $ HeadingLevel level
      else fail "Invalid heading level"

data Heading = Heading HeadingLevel [TextNode] deriving (Show, Eq)

newtype NoteId = NoteId T.Text deriving (Show, Eq)

instance FromJSON NoteId where
  parseJSON = withText "NoteId" $ \t -> do
    noteId <- parseNonEmpty "id" t
    pure $ NoteId noteId

data BlockType
  = ParagraphType
  | HeadingType
  | CodeBlockType
  | BlockQuoteType
  | BulletListType
  | OrderedListType
  | ListItemType
  | NoteContentType
  deriving (Show, Eq)

instance FromJSON BlockType where
  parseJSON :: Value -> Parser BlockType
  parseJSON = withText "BlockType" $ \t -> case t of
    "paragraph" -> pure ParagraphType
    "heading" -> pure HeadingType
    "code_block" -> pure CodeBlockType
    "blockquote" -> pure BlockQuoteType
    "bullet_list" -> pure BulletListType
    "ordered_list" -> pure OrderedListType
    "list_item" -> pure ListItemType
    "note_content" -> pure NoteContentType
    _ -> fail "Invalid block type"

instance ToJSON BlockType where
  toJSON :: BlockType -> Value
  toJSON bt = case bt of
    ParagraphType -> String "paragraph"
    HeadingType -> String "heading"
    CodeBlockType -> String "code_block"
    BlockQuoteType -> String "blockquote"
    BulletListType -> String "bullet_list"
    OrderedListType -> String "ordered_list"
    ListItemType -> String "list_item"
    NoteContentType -> String "note_content"
