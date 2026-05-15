{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module ProseMirror.Model (BlockNode (..), Block (..), TextNode (..), InlineNode (..), Mark (..), Link (..), Meta, Node (..), NoteId (..), HeadingLevel (..), PMDoc (..), NodeType (..), CodeBlockLanguage (..), Image (..), assertRootNodeIsDoc, isRootBlockNode, isAtomNode, isLeafBlockNode, wrapChildrenToBlock, parseProseMirror, parseProseMirrorText) where

import Control.Monad ((>=>))
import Data.Aeson (FromJSON (parseJSON), Object, ToJSON (toJSON), Value (..), eitherDecode, eitherDecodeStrictText, object, withObject, withScientific, withText, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BL
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import ProseMirror.Utils.Json (parseNonEmpty)

-- TODO: Make linkTitle optional
data Link = Link {url :: T.Text, linkTitle :: T.Text} deriving (Show, Eq)

type Meta = T.Text

instance FromJSON Link where
  parseJSON = withObject "Link" $ \v -> do
    linkUrl <- v .: "href" >>= parseNonEmpty "href"
    title <- v .: "title"
    pure Link {url = linkUrl, linkTitle = title}

instance ToJSON Link where
  toJSON link = object ["href" .= url link, "title" .= linkTitle link]

data Image = Image {src :: T.Text, alt :: Maybe T.Text, imageTitle :: Maybe T.Text} deriving (Show, Eq)

instance FromJSON Image where
  parseJSON = withObject "Image" $ \v -> do
    imgSrc <- v .: "src" >>= parseNonEmpty "src"
    imgAlt <- v .:? "alt"
    imgTitle <- v .:? "title"
    pure Image {src = imgSrc, alt = imgAlt, imageTitle = imgTitle}

instance ToJSON Image where
  toJSON img = object ["src" .= src img, "alt" .= alt img, "title" .= imageTitle img]

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
  toJSON textNode =
    object $
      catMaybes
        [ Just $ "type" .= T.pack "text",
          Just $ "text" .= text textNode,
          fmap ("marks" .=) (marks textNode)
        ]

newtype HeadingLevel = HeadingLevel Int deriving (Show, Eq)

instance FromJSON HeadingLevel where
  parseJSON = withScientific "HeadingLevel" $ \n -> do
    let level = floor n
    if level >= 1 && level <= 6
      then pure $ HeadingLevel level
      else fail "Invalid heading level"

newtype CodeBlockLanguage = CodeBlockLanguage T.Text deriving (Show, Eq)

instance FromJSON CodeBlockLanguage where
  parseJSON = withText "CodeBlockLanguage" $ \language -> pure $ CodeBlockLanguage language

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
  | HorizontalRuleType
  | ImageType
  | FigureType
  | FigureContentType
  | CaptionType
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
    "horizontal_rule" -> pure HorizontalRuleType
    "image" -> pure ImageType
    "figure" -> pure FigureType
    "figure_content" -> pure FigureContentType
    "caption" -> pure CaptionType
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
    HorizontalRuleType -> String "horizontal_rule"
    ImageType -> String "image"
    FigureType -> String "figure"
    FigureContentType -> String "figure_content"
    CaptionType -> String "caption"
    NoteRefType -> String "note_ref"
    NoteContentType -> String "note_content"

data Block = Doc (Maybe Meta) | Paragraph | Heading HeadingLevel | CodeBlock (Maybe CodeBlockLanguage) | BlockQuote | BulletList | OrderedList | ListItem | HorizontalRule | Figure | FigureContent | Caption | NoteContent NoteId deriving (Show, Eq)

data BlockNode = PMBlock {block :: Block, content :: Maybe [Node]} deriving (Show, Eq)

-- Mirrors PM's inline group: a text run plus the inline-typed nodes that carry attrs.
-- These are leaf nodes — they don't hold child content — so this is a flat sum, not a record.
data InlineNode = InlineText TextNode | InlineImage Image | NoteRef NoteId deriving (Show, Eq)

data Node = BlockNode BlockNode | InlineNode InlineNode deriving (Show, Eq)

nodeType :: Node -> NodeType
nodeType (BlockNode (PMBlock (Doc _) _)) = DocType
nodeType (BlockNode (PMBlock Paragraph _)) = ParagraphType
nodeType (BlockNode (PMBlock (Heading _) _)) = HeadingType
nodeType (BlockNode (PMBlock (CodeBlock _) _)) = CodeBlockType
nodeType (BlockNode (PMBlock BlockQuote _)) = BlockQuoteType
nodeType (BlockNode (PMBlock BulletList _)) = BulletListType
nodeType (BlockNode (PMBlock OrderedList _)) = OrderedListType
nodeType (BlockNode (PMBlock ListItem _)) = ListItemType
nodeType (BlockNode (PMBlock HorizontalRule _)) = HorizontalRuleType
nodeType (BlockNode (PMBlock Figure _)) = FigureType
nodeType (BlockNode (PMBlock FigureContent _)) = FigureContentType
nodeType (BlockNode (PMBlock Caption _)) = CaptionType
nodeType (BlockNode (PMBlock (NoteContent _) _)) = NoteContentType
nodeType (InlineNode (InlineText _)) = TextType
nodeType (InlineNode (InlineImage _)) = ImageType
nodeType (InlineNode (NoteRef _)) = NoteRefType

-- In ProseMirror nodes like note refs and images which don't have directly editable content
-- and should be treated as a single unit in the view have the `atom` property set to `true`.
-- https://prosemirror.net/docs/ref/#model.NodeSpec.atom
isAtomNode :: Node -> Bool
isAtomNode (InlineNode (InlineImage _)) = True
isAtomNode (InlineNode (NoteRef _)) = True
isAtomNode _ = False

isLeafBlockNode :: Node -> Bool
isLeafBlockNode (BlockNode (PMBlock HorizontalRule _)) = True
isLeafBlockNode _ = False

instance FromJSON Node where
  parseJSON = withObject "Node" $ \v -> do
    nType <- (v .: "type" :: Parser NodeType)
    children <- v .:? "content"
    case nType of
      DocType -> do
        nAttrs <- (v .:? "attrs" :: Parser (Maybe Object))
        case nAttrs of
          Just attrs -> do
            meta <- (attrs .:? "pandocMeta" :: Parser (Maybe Meta))
            pure $ BlockNode $ PMBlock {block = Doc meta, content = children}
          Nothing -> pure $ BlockNode $ PMBlock {block = Doc Nothing, content = children}
      TextType -> do
        nText <- v .: "text" >>= parseNonEmpty "text"
        nMarks <- v .:? "marks"
        pure $ InlineNode $ InlineText $ PMText {text = nText, marks = nMarks}
      ParagraphType -> pure $ BlockNode $ PMBlock {block = Paragraph, content = children}
      HeadingType -> do
        nAttrs <- (v .:? "attrs" :: Parser (Maybe Object))
        case nAttrs of
          Just attrs -> do
            level <- (attrs .: "level" :: Parser HeadingLevel)
            pure $ BlockNode $ PMBlock {block = Heading level, content = children}
          Nothing -> fail "Could not find heading attrs"
      CodeBlockType -> do
        nAttrs <- (v .:? "attrs" :: Parser (Maybe Object))
        case nAttrs of
          Just attrs -> do
            language <- (attrs .:? "language" :: Parser (Maybe CodeBlockLanguage))
            pure $ BlockNode $ PMBlock {block = CodeBlock language, content = children}
          Nothing -> pure $ BlockNode $ PMBlock {block = CodeBlock Nothing, content = children}
      BlockQuoteType -> pure $ BlockNode $ PMBlock {block = BlockQuote, content = children}
      BulletListType -> pure $ BlockNode $ PMBlock {block = BulletList, content = children}
      OrderedListType -> pure $ BlockNode $ PMBlock {block = OrderedList, content = children}
      ListItemType -> pure $ BlockNode $ PMBlock {block = ListItem, content = children}
      HorizontalRuleType -> pure $ BlockNode $ PMBlock {block = HorizontalRule, content = Nothing}
      ImageType -> do
        nAttrs <- v .:? "attrs"
        case nAttrs of
          Just attrs -> fmap (InlineNode . InlineImage) (parseJSON attrs)
          Nothing -> fail "Could not find image attrs"
      FigureType -> pure $ BlockNode $ PMBlock {block = Figure, content = children}
      FigureContentType -> pure $ BlockNode $ PMBlock {block = FigureContent, content = children}
      CaptionType -> pure $ BlockNode $ PMBlock {block = Caption, content = children}
      NoteRefType -> do
        nAttrs <- (v .:? "attrs" :: Parser (Maybe Object))
        case nAttrs of
          Just attrs -> do
            noteId <- (attrs .: "id" :: Parser NoteId)
            pure $ InlineNode $ NoteRef noteId
          Nothing -> fail "Could not find note ref attrs"
      NoteContentType -> do
        nAttrs <- (v .:? "attrs" :: Parser (Maybe Object))
        case nAttrs of
          Just attrs -> do
            noteId <- (attrs .: "id" :: Parser NoteId)
            pure $ BlockNode $ PMBlock {block = NoteContent noteId, content = children}
          Nothing -> fail "Could not find note ref attrs"

instance ToJSON Node where
  toJSON (InlineNode inlineNode) = toJSON inlineNode
  toJSON (BlockNode (PMBlock bl children)) = case bl of
    Doc Nothing -> object ["type" .= toJSON DocType, "content" .= children]
    Doc meta -> object ["type" .= toJSON DocType, "content" .= children, "attrs" .= object ["pandocMeta" .= toJSON meta]]
    Paragraph -> object ["type" .= toJSON ParagraphType, "content" .= children]
    Heading (HeadingLevel level) -> object ["type" .= toJSON HeadingType, "content" .= children, "attrs" .= object ["level" .= toJSON level]]
    CodeBlock Nothing -> object ["type" .= toJSON CodeBlockType, "content" .= children]
    CodeBlock (Just (CodeBlockLanguage language)) -> object ["type" .= toJSON CodeBlockType, "content" .= children, "attrs" .= object ["language" .= toJSON language]]
    BlockQuote -> object ["type" .= toJSON BlockQuoteType, "content" .= children]
    BulletList -> object ["type" .= toJSON BulletListType, "content" .= children]
    OrderedList -> object ["type" .= toJSON OrderedListType, "content" .= children]
    ListItem -> object ["type" .= toJSON ListItemType, "content" .= children]
    HorizontalRule -> object ["type" .= toJSON HorizontalRuleType, "content" .= (Nothing :: Maybe [Node])]
    Figure -> object ["type" .= toJSON FigureType, "content" .= children]
    FigureContent -> object ["type" .= toJSON FigureContentType, "content" .= children]
    Caption -> object ["type" .= toJSON CaptionType, "content" .= children]
    NoteContent (NoteId noteId) -> object ["type" .= toJSON NoteContentType, "content" .= children, "attrs" .= object ["id" .= toJSON noteId]]

instance ToJSON InlineNode where
  toJSON (InlineText textNode) = toJSON textNode
  toJSON (InlineImage img) = object ["type" .= toJSON ImageType, "attrs" .= toJSON img]
  toJSON (NoteRef (NoteId noteId)) = object ["type" .= toJSON NoteRefType, "attrs" .= object ["id" .= toJSON noteId]]

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