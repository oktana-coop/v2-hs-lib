{-# LANGUAGE OverloadedStrings #-}

module ProseMirror.Decoration (Decoration (..), InlineDecoration (..), NodeDecoration (..), WidgetDecoration (..), DecorationAttrs (..), undecorate) where

import Data.Aeson (ToJSON, object, toJSON, (.=))
import qualified Data.Text as T

data DecorationAttrs = DecorationAttrs {nodeName :: Maybe T.Text, cssClass :: Maybe T.Text, style :: Maybe T.Text} deriving (Show, Eq)

instance ToJSON DecorationAttrs where
  toJSON decAttrs = object ["nodeName" .= nodeName decAttrs, "class" .= cssClass decAttrs, "style" .= style decAttrs]

data InlineDecoration a = PMInlineDecoration {inlineDecFrom :: Int, inlineDecTo :: Int, inlineDecAttrs :: DecorationAttrs, inlineDecContent :: a} deriving (Show, Eq)

instance ToJSON (InlineDecoration a) where
  toJSON dec = object ["type" .= T.pack "inline", "from" .= inlineDecFrom dec, "to" .= inlineDecTo dec, "attrs" .= inlineDecAttrs dec]

data NodeDecoration a = PMNodeDecoration {nodeDecFrom :: Int, nodeDecTo :: Int, nodeDecAttrs :: DecorationAttrs, nodeDecContent :: a} deriving (Show, Eq)

instance ToJSON (NodeDecoration a) where
  toJSON dec = object ["type" .= T.pack "inline", "from" .= nodeDecFrom dec, "to" .= nodeDecTo dec, "attrs" .= nodeDecAttrs dec]

data WidgetDecoration a = PMWidgetDecoration {pos :: Int, widgetDecContent :: a} deriving (Show, Eq)

instance (ToJSON a) => ToJSON (WidgetDecoration a) where
  toJSON dec = object ["type" .= T.pack "widget", "pos" .= pos dec, "node" .= toJSON (widgetDecContent dec)]

data Decoration a = InlineDecoration (InlineDecoration a) | NodeDecoration (NodeDecoration a) | WidgetDecoration (WidgetDecoration a) deriving (Show, Eq)

undecorate :: Decoration a -> a
undecorate (InlineDecoration dec) = inlineDecContent dec
undecorate (NodeDecoration dec) = nodeDecContent dec
undecorate (WidgetDecoration dec) = widgetDecContent dec

instance (ToJSON a) => ToJSON (Decoration a) where
  toJSON (InlineDecoration inlineDec) = toJSON inlineDec
  toJSON (NodeDecoration nodeDec) = toJSON nodeDec
  toJSON (WidgetDecoration widgetDec) = toJSON widgetDec