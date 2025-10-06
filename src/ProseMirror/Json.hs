{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module ProseMirror.Json (BlockNode (..), TextNode (..), Mark (..), Node (..), PMDoc (..), isRootBlockNode, isAtomNode, wrapChildrenToBlock, parseProseMirror, parseProseMirrorText) where

import Control.Monad ((>=>))
import Data.Aeson (FromJSON (parseJSON), Object, ToJSON (..), eitherDecodeStrictText, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Decoding (eitherDecode)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BL
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import ProseMirror.Utils.Json (parseNonEmpty)

data Node = BlockNode BlockNode | TextNode TextNode deriving (Show, Eq)

instance FromJSON Node where
  parseJSON val =
    ( withObject "Node" $ \v -> do
        elementType <- (v .: "type" :: Parser String)
        case elementType of
          "text" -> fmap TextNode $ parseJSON val
          _ -> fmap BlockNode $ parseJSON val
    )
      val

instance ToJSON Node where
  toJSON (BlockNode blockNode) = toJSON blockNode
  toJSON (TextNode textNode) = toJSON textNode

data Mark = PMMark {markType :: T.Text, markAttrs :: Maybe Object} deriving (Show, Eq)

instance FromJSON Mark where
  parseJSON = withObject "Mark" $ \v -> do
    mType <- v .: "type" >>= parseNonEmpty "type"
    mAttrs <- v .:? "attrs"
    pure $ PMMark {markType = mType, markAttrs = mAttrs}

instance ToJSON Mark where
  toJSON mark = object $ ["type" .= markType mark, "attrs" .= markAttrs mark]

data TextNode = PMText {text :: T.Text, marks :: Maybe (NonEmpty Mark)} deriving (Show, Eq)

instance FromJSON TextNode where
  parseJSON = withObject "TextNode" $ \v -> do
    nText <- v .: "text" >>= parseNonEmpty "text"
    nMarks <- v .:? "marks"
    pure $ PMText {text = nText, marks = nMarks}

instance ToJSON TextNode where
  toJSON textNode = object $ ["type" .= T.pack "text", "text" .= text textNode, "marks" .= marks textNode]

data BlockNode = PMBlock {nodeType :: T.Text, content :: Maybe [Node], attrs :: Maybe Object} deriving (Show, Eq)

instance FromJSON BlockNode where
  parseJSON = withObject "BlockNode" $ \v -> do
    nType <- v .: "type" >>= parseNonEmpty "type"
    nContent <- v .:? "content"
    nAttrs <- v .:? "attrs"
    pure PMBlock {nodeType = nType, content = nContent, attrs = nAttrs}

instance ToJSON BlockNode where
  toJSON blockNode = object $ ["type" .= nodeType blockNode, "content" .= content blockNode, "attrs" .= attrs blockNode]

data PMDoc = PMDoc {doc :: BlockNode} deriving (Show, Eq)

instance ToJSON PMDoc where
  toJSON decoratedPMDoc = object ["doc" .= doc decoratedPMDoc]

isRootBlockNode :: BlockNode -> Bool
isRootBlockNode blockNode = nodeType blockNode == "doc"

-- In ProseMirror nodes like note refs which don't have directly editable content and
-- should be treated as a single unit in the view have the `atom` property set to `true`.
-- https://prosemirror.net/docs/ref/#model.NodeSpec.atom
isAtomNode :: BlockNode -> Bool
isAtomNode blockNode = nodeType blockNode == "note_ref"

wrapChildrenToBlock :: BlockNode -> [Node] -> BlockNode
wrapChildrenToBlock (PMBlock blockType Nothing blockAttrs) children = PMBlock blockType (Just children) blockAttrs
wrapChildrenToBlock (PMBlock blockType (Just existingChildren) blockAttrs) newChildren = PMBlock blockType (Just (existingChildren <> newChildren)) blockAttrs

-- Using Kleisli composition to compose the 2 smaller functions in the monadic context (Either monad)
parseProseMirror :: BL.ByteString -> Either String PMDoc
parseProseMirror = eitherDecode >=> assertRootNodeIsDoc

-- Using Kleisli composition to compose the 2 smaller functions in the monadic context (Either monad)
parseProseMirrorText :: T.Text -> Either String PMDoc
parseProseMirrorText = eitherDecodeStrictText >=> assertRootNodeIsDoc

assertRootNodeIsDoc :: Node -> Either String PMDoc
assertRootNodeIsDoc (BlockNode rootNode) =
  if nodeType rootNode == "doc"
    then Right $ PMDoc {doc = rootNode}
    else Left "Root node type is not doc"
assertRootNodeIsDoc _ = Left "Root node is not a block node."