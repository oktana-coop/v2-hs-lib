{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module ProseMirror.PMJson (BlockNode (..), TextNode (..), Mark (..), Node (..), PMDoc (..), isRootBlockNode, wrapChildrenToBlock) where

import Data.Aeson (Object, ToJSON (..), object, (.=))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T

data Node = BlockNode BlockNode | TextNode TextNode deriving (Show, Eq)

instance ToJSON Node where
  toJSON (BlockNode blockNode) = toJSON blockNode
  toJSON (TextNode textNode) = toJSON textNode

data Mark = PMMark {markType :: T.Text, markAttrs :: Maybe Object} deriving (Show, Eq)

instance ToJSON Mark where
  toJSON mark = object $ ["type" .= markType mark, "attrs" .= markAttrs mark]

data TextNode = PMText {text :: T.Text, marks :: Maybe (NonEmpty Mark)} deriving (Show, Eq)

instance ToJSON TextNode where
  toJSON textNode = object $ ["type" .= T.pack "text", "text" .= text textNode, "marks" .= marks textNode]

data BlockNode = PMBlock {nodeType :: T.Text, content :: Maybe [Node], attrs :: Maybe Object} deriving (Show, Eq)

instance ToJSON BlockNode where
  toJSON blockNode = object $ ["type" .= nodeType blockNode, "content" .= content blockNode, "attrs" .= attrs blockNode]

data PMDoc = PMDoc {doc :: BlockNode} deriving (Show, Eq)

instance ToJSON PMDoc where
  toJSON decoratedPMDoc = object ["doc" .= doc decoratedPMDoc]

isRootBlockNode :: BlockNode -> Bool
isRootBlockNode blockNode = nodeType blockNode == "doc"

wrapChildrenToBlock :: BlockNode -> [Node] -> BlockNode
wrapChildrenToBlock (PMBlock blockType Nothing blockAttrs) children = PMBlock blockType (Just children) blockAttrs
wrapChildrenToBlock (PMBlock blockType (Just existingChildren) blockAttrs) newChildren = PMBlock blockType (Just (existingChildren <> newChildren)) blockAttrs
