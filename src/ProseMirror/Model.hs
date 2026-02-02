{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module ProseMirror.Model (BlockNode (..), Block (..), TextNode (..), Mark (..), Link (..), Node (..), NoteId (..), HeadingLevel (..), PMDoc (..), NodeType (..), assertRootNodeIsDoc, isRootBlockNode, isAtomNode, wrapChildrenToBlock, parseProseMirror, parseProseMirrorText) where

import Control.Monad ((>=>))
import Data.Aeson (FromJSON (parseJSON), Object, ToJSON (toJSON), Value (..), eitherDecode, eitherDecodeStrictText, object, withObject, withScientific, withText, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BL
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

instance ToJSON TextNode where
  toJSON textNode = object $ ["type" .= T.pack "text", "text" .= text textNode, "marks" .= marks textNode]

newtype HeadingLevel = HeadingLevel Int deriving (Show, Eq)

instance FromJSON HeadingLevel where
  parseJSON = withScientific "HeadingLevel" $ \n -> do
    let level = floor n
    if level >= 1 && level <= 6
      then pure $ HeadingLevel level
      else fail "Invalid heading level"

newtype NoteId = NoteId T.Text deriving (Show, Eq)

instance FromJSON NoteId where
  parseJSON = withText "NoteId" $ \t -> do
    noteId <- parseNonEmpty "id" t
    pure $ NoteId noteId

data NodeType
  = DocType
  | TextType
  | ParagraphType
  | HeadingType
  | CodeBlockType
  | BlockQuoteType
  | BulletListType
  | OrderedListType
  | ListItemType
  | NoteRefType
  | NoteContentType
  deriving (Show, Eq)

instance FromJSON NodeType where
  parseJSON :: Value -> Parser NodeType
  parseJSON = withText "BlockType" $ \t -> case t of
    "doc" -> pure DocType
    "text" -> pure TextType
    "paragraph" -> pure ParagraphType
    "heading" -> pure HeadingType
    "code_block" -> pure CodeBlockType
    "blockquote" -> pure BlockQuoteType
    "bullet_list" -> pure BulletListType
    "ordered_list" -> pure OrderedListType
    "list_item" -> pure ListItemType
    "note_ref" -> pure NoteRefType
    "note_content" -> pure NoteContentType
    _ -> fail "Invalid block type"

instance ToJSON NodeType where
  toJSON :: NodeType -> Value
  toJSON bt = case bt of
    DocType -> String "doc"
    TextType -> String "text"
    ParagraphType -> String "paragraph"
    HeadingType -> String "heading"
    CodeBlockType -> String "code_block"
    BlockQuoteType -> String "blockquote"
    BulletListType -> String "bullet_list"
    OrderedListType -> String "ordered_list"
    ListItemType -> String "list_item"
    NoteRefType -> String "note_ref"
    NoteContentType -> String "note_content"

data Block = Doc | Paragraph | Heading HeadingLevel | CodeBlock | BlockQuote | BulletList | OrderedList | ListItem | NoteRef NoteId | NoteContent NoteId deriving (Show, Eq)

data BlockNode = PMBlock {block :: Block, content :: Maybe [Node]} deriving (Show, Eq)

data Node = BlockNode BlockNode | TextNode TextNode deriving (Show, Eq)

nodeType :: Node -> NodeType
nodeType (BlockNode (PMBlock Doc _)) = DocType
nodeType (BlockNode (PMBlock Paragraph _)) = ParagraphType
nodeType (BlockNode (PMBlock (Heading _) _)) = HeadingType
nodeType (BlockNode (PMBlock CodeBlock _)) = CodeBlockType
nodeType (BlockNode (PMBlock BlockQuote _)) = BlockQuoteType
nodeType (BlockNode (PMBlock BulletList _)) = BulletListType
nodeType (BlockNode (PMBlock OrderedList _)) = OrderedListType
nodeType (BlockNode (PMBlock ListItem _)) = ListItemType
nodeType (BlockNode (PMBlock (NoteRef _) _)) = NoteRefType
nodeType (BlockNode (PMBlock (NoteContent _) _)) = NoteContentType
nodeType (TextNode _) = TextType

-- In ProseMirror nodes like note refs which don't have directly editable content and
-- should be treated as a single unit in the view have the `atom` property set to `true`.
-- https://prosemirror.net/docs/ref/#model.NodeSpec.atom
isAtomNode :: Node -> Bool
isAtomNode (BlockNode (PMBlock (NoteRef _) _)) = True
isAtomNode _ = False

instance FromJSON Node where
  parseJSON = withObject "Node" $ \v -> do
    nType <- (v .: "type" :: Parser NodeType)
    children <- v .:? "content"
    case nType of
      DocType -> pure $ BlockNode $ PMBlock {block = Doc, content = children}
      TextType -> do
        nText <- v .: "text" >>= parseNonEmpty "text"
        nMarks <- v .:? "marks"
        pure $ TextNode $ PMText {text = nText, marks = nMarks}
      ParagraphType -> pure $ BlockNode $ PMBlock {block = Paragraph, content = children}
      HeadingType -> do
        nAttrs <- (v .:? "attrs" :: Parser (Maybe Object))
        case nAttrs of
          Just attrs -> do
            level <- (attrs .: "level" :: Parser HeadingLevel)
            pure $ BlockNode $ PMBlock {block = Heading level, content = children}
          Nothing -> fail "Could not find heading attrs"
      CodeBlockType -> pure $ BlockNode $ PMBlock {block = CodeBlock, content = children}
      BlockQuoteType -> pure $ BlockNode $ PMBlock {block = BlockQuote, content = children}
      BulletListType -> pure $ BlockNode $ PMBlock {block = BulletList, content = children}
      OrderedListType -> pure $ BlockNode $ PMBlock {block = OrderedList, content = children}
      ListItemType -> pure $ BlockNode $ PMBlock {block = ListItem, content = children}
      NoteRefType -> do
        nAttrs <- (v .:? "attrs" :: Parser (Maybe Object))
        case nAttrs of
          Just attrs -> do
            noteId <- (attrs .: "id" :: Parser NoteId)
            pure $ BlockNode $ PMBlock {block = NoteRef noteId, content = children}
          Nothing -> fail "Could not find note ref attrs"
      NoteContentType -> do
        nAttrs <- (v .:? "attrs" :: Parser (Maybe Object))
        case nAttrs of
          Just attrs -> do
            noteId <- (attrs .: "id" :: Parser NoteId)
            pure $ BlockNode $ PMBlock {block = NoteContent noteId, content = children}
          Nothing -> fail "Could not find note ref attrs"

instance ToJSON Node where
  toJSON (TextNode textNode) = toJSON textNode
  toJSON (BlockNode (PMBlock bl children)) = case bl of
    Doc -> object ["type" .= toJSON DocType, "content" .= children]
    Paragraph -> object ["type" .= toJSON ParagraphType, "content" .= children]
    Heading (HeadingLevel level) -> object ["type" .= toJSON HeadingType, "content" .= children, "attrs" .= object ["level" .= toJSON level]]
    CodeBlock -> object ["type" .= toJSON CodeBlockType, "content" .= children]
    BlockQuote -> object ["type" .= toJSON BlockQuoteType, "content" .= children]
    BulletList -> object ["type" .= toJSON BulletListType, "content" .= children]
    OrderedList -> object ["type" .= toJSON OrderedListType, "content" .= children]
    ListItem -> object ["type" .= toJSON ListItemType, "content" .= children]
    NoteRef (NoteId noteId) -> object ["type" .= toJSON NoteRefType, "content" .= children, "attrs" .= object ["id" .= toJSON noteId]]
    NoteContent (NoteId noteId) -> object ["type" .= toJSON NoteContentType, "content" .= children, "attrs" .= object ["id" .= toJSON noteId]]

data PMDoc = PMDoc {doc :: Node} deriving (Show, Eq)

instance ToJSON PMDoc where
  toJSON pmDoc = toJSON $ doc pmDoc

isRootBlockNode :: Node -> Bool
isRootBlockNode blockNode = nodeType blockNode == DocType

wrapChildrenToBlock :: BlockNode -> [Node] -> BlockNode
wrapChildrenToBlock (PMBlock bl Nothing) children = PMBlock bl (Just children)
wrapChildrenToBlock (PMBlock bl (Just existingChildren)) newChildren = PMBlock bl (Just (existingChildren <> newChildren))

-- Using Kleisli composition to compose the 2 smaller functions in the monadic context (Either monad)
parseProseMirror :: BL.ByteString -> Either String PMDoc
parseProseMirror = eitherDecode >=> assertRootNodeIsDoc

-- Using Kleisli composition to compose the 2 smaller functions in the monadic context (Either monad)
parseProseMirrorText :: T.Text -> Either String PMDoc
parseProseMirrorText = eitherDecodeStrictText >=> assertRootNodeIsDoc

assertRootNodeIsDoc :: Node -> Either String PMDoc
assertRootNodeIsDoc n =
  if nodeType n == DocType
    then Right $ PMDoc {doc = n}
    else Left "Root node type is not doc"