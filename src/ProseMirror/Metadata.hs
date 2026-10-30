{-# LANGUAGE OverloadedStrings #-}

module ProseMirror.Metadata (addDocMeta, metaFromPMDoc, metaFromJsonText, metaToJsonText) where

import Data.Aeson (eitherDecodeStrictText, pairs, toEncoding, (.=))
import Data.Aeson.Decoding.Text (textToTokens)
import Data.Aeson.Decoding.Tokens (TkArray (..), TkRecord (..), Tokens (..))
import Data.Aeson.Encoding (Encoding, Series, encodingToLazyByteString, list, pair)
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Metadata.KeyOutline (KeyOutline (..), addKeyOutline, listItemsWithOutlines, mapEntriesInOutlineOrder, stripTempMetaOutlineKey, takeKeyOutline)
import qualified ProseMirror.Model as PM (Block (..), BlockNode (..), Node (..), PMDoc (..))
import Text.Pandoc.Definition (Meta (..), MetaValue (..), isNullMeta, nullMeta)

-- The doc node with the metadata in its attribute, or none for empty metadata. Other nodes are left as they are.
addDocMeta :: Meta -> PM.Node -> PM.Node
addDocMeta meta (PM.BlockNode (PM.PMBlock (PM.Doc _) content)) = PM.BlockNode (PM.PMBlock docWithMeta content)
  where
    docWithMeta :: PM.Block
    docWithMeta
      | isNullMeta (stripTempMetaOutlineKey meta) = PM.Doc Nothing
      | otherwise = PM.Doc (Just (metaToJsonText meta))
addDocMeta _ node = node

-- The metadata in the doc's attribute, or empty metadata if it has none.
metaFromPMDoc :: PM.PMDoc -> Either String Meta
metaFromPMDoc (PM.PMDoc (PM.BlockNode (PM.PMBlock (PM.Doc (Just pmMeta)) _))) = metaFromJsonText pmMeta
metaFromPMDoc _ = Right nullMeta

-- Pandoc's metadata parsed from its JSON text, with an extra temporary key that holds the order of the keys.
metaFromJsonText :: T.Text -> Either String Meta
metaFromJsonText json = fmap (addKeyOutline jsonKeyOutline) (eitherDecodeStrictText json)
  where
    -- aeson's `Value` does not keep the order of the keys of an object, but its token stream has them in the order of the text.
    jsonKeyOutline :: KeyOutline
    jsonKeyOutline = case parseOrderedJson json of
      Just jsonValue -> objectOutline jsonValue
      Nothing -> NoOutline

    objectOutline :: OrderedJson -> KeyOutline
    objectOutline (JsonObject entries) = MapOutline (map entryOutline entries)
    objectOutline _ = NoOutline

    entryOutline :: (T.Text, OrderedJson) -> (T.Text, KeyOutline)
    entryOutline (key, value) = (key, valueOutline value)

    -- The outline of a tagged value, e.g. {"t": "MetaMap", "c": {...}}: the one of its contents.
    valueOutline :: OrderedJson -> KeyOutline
    valueOutline (JsonObject fields) = case (lookup "t" fields, lookup "c" fields) of
      (Just (JsonString "MetaMap"), Just contents) -> objectOutline contents
      (Just (JsonString "MetaList"), Just (JsonArray items)) -> ListOutline (map valueOutline items)
      _ -> NoOutline
    valueOutline _ = NoOutline

-- A JSON value with the keys of each object in the order of the text.
data OrderedJson = JsonObject [(T.Text, OrderedJson)] | JsonArray [OrderedJson] | JsonString T.Text | JsonScalar

-- A JSON text as a value with the keys of each object in the order of the text, if the text is valid JSON.
parseOrderedJson :: T.Text -> Maybe OrderedJson
parseOrderedJson text = case orderedJson (textToTokens text) of
  Right (jsonValue, _) -> Just jsonValue
  Left _ -> Nothing

-- Reads one JSON value from a token stream, and returns it with the rest of the stream after it.
orderedJson :: Tokens k e -> Either e (OrderedJson, k)
orderedJson (TkLit _ rest) = Right (JsonScalar, rest)
orderedJson (TkText t rest) = Right (JsonString t, rest)
orderedJson (TkNumber _ rest) = Right (JsonScalar, rest)
orderedJson (TkArrayOpen tokens) = do
  (items, rest) <- arrayItems tokens
  pure (JsonArray items, rest)
orderedJson (TkRecordOpen tokens) = do
  (fields, rest) <- recordFields tokens
  pure (JsonObject fields, rest)
orderedJson (TkErr err) = Left err

arrayItems :: TkArray k e -> Either e ([OrderedJson], k)
arrayItems (TkItem tokens) = do
  (item, rest) <- orderedJson tokens
  (items, end) <- arrayItems rest
  pure (item : items, end)
arrayItems (TkArrayEnd rest) = Right ([], rest)
arrayItems (TkArrayErr err) = Left err

recordFields :: TkRecord k e -> Either e ([(T.Text, OrderedJson)], k)
recordFields (TkPair key tokens) = do
  (value, rest) <- orderedJson tokens
  (fields, end) <- recordFields rest
  pure ((Key.toText key, value) : fields, end)
recordFields (TkRecordEnd rest) = Right ([], rest)
recordFields (TkRecordErr err) = Left err

-- Pandoc's JSON for the metadata, with the keys of each map in the order of the metadata's key outline.
metaToJsonText :: Meta -> T.Text
metaToJsonText meta = encodingToText (mapEncodingInOutlineOrder metaOutline (unMeta metaWithoutOutline))
  where
    (metaOutline, metaWithoutOutline) = takeKeyOutline meta

    mapEncodingInOutlineOrder :: KeyOutline -> M.Map T.Text MetaValue -> Encoding
    mapEncodingInOutlineOrder outline metaMap = pairs (foldMap entryEncoding (mapEntriesInOutlineOrder outline metaMap))

    entryEncoding :: (T.Text, MetaValue, KeyOutline) -> Series
    entryEncoding (key, value, valueOutline) = pair (Key.fromText key) (valueEncodingInOutlineOrder valueOutline value)

    -- The tagged encoding of pandoc-types, e.g. {"t": "MetaMap", "c": {...}}.
    valueEncodingInOutlineOrder :: KeyOutline -> MetaValue -> Encoding
    valueEncodingInOutlineOrder outline (MetaMap metaMap) = taggedEncoding "MetaMap" (mapEncodingInOutlineOrder outline metaMap)
    valueEncodingInOutlineOrder outline (MetaList items) = taggedEncoding "MetaList" (list itemEncoding (listItemsWithOutlines outline items))
    valueEncodingInOutlineOrder _ value = toEncoding value

    itemEncoding :: (MetaValue, KeyOutline) -> Encoding
    itemEncoding (item, itemOutline) = valueEncodingInOutlineOrder itemOutline item

    taggedEncoding :: T.Text -> Encoding -> Encoding
    taggedEncoding tag contents = pairs ("t" .= tag <> pair "c" contents)

    encodingToText :: Encoding -> T.Text
    encodingToText = TE.decodeUtf8 . BL.toStrict . encodingToLazyByteString
