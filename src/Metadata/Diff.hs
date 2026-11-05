{-# LANGUAGE OverloadedStrings #-}

module Metadata.Diff (metadataDiffToJSON) where

import Data.Aeson (Value, object, toJSON, (.=))
import Data.Aeson.Types (Pair)
import Data.List (find)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Tree (Tree (..))
import NodeDiff.Common (MetaNodeDiff (..), MetaValueDiff (..), NodeOp (..), toNode)
import NodeDiff.LeafTextSpans (DocNodeDiff (..))
import RichTextDiff (hasChanges)
import Text.Pandoc.Definition (MetaValue (..))

metadataDiffToJSON :: Tree DocNodeDiff -> Value
metadataDiffToJSON = toJSON . keyDiffsToJSON . metadataKeyDiffs

metadataKeyDiffs :: Tree DocNodeDiff -> [Tree DocNodeDiff]
metadataKeyDiffs diff = maybe [] subForest (find isMetadata (subForest diff))
  where
    isMetadata :: Tree DocNodeDiff -> Bool
    isMetadata = (== MetadataDiff) . rootLabel

-- The tree's type allows any type of meta/body doc node because metadata may contain arbitrary rich text. Here we
-- assume that the diff tree is correctly shaped by pandoc-tree, which means that Metadata can only have key nodes
-- as direct children. Values are modeled as children of these keys.
keyDiffsToJSON :: [Tree DocNodeDiff] -> [Value]
keyDiffsToJSON = mapMaybe keyDiffToJSON
  where
    keyDiffToJSON :: Tree DocNodeDiff -> Maybe Value
    keyDiffToJSON (Node (MetaNodeDiff (MetaKeyDiff keyOp)) values) =
      Just (object ["op" .= nodeOpName keyOp, "key" .= toNode keyOp, "values" .= valueDiffsToJSON values])
    keyDiffToJSON _ = Nothing

valueDiffsToJSON :: [Tree DocNodeDiff] -> [Value]
valueDiffsToJSON = mapMaybe valueDiffToJSON
  where
    valueDiffToJSON :: Tree DocNodeDiff -> Maybe Value
    valueDiffToJSON valueTree@(Node (MetaNodeDiff (PandocMetaValueDiff valueOp)) _) =
      Just (object (("op" .= nodeOpName valueOp) : valueFields valueTree (toNode valueOp)))
    valueDiffToJSON (Node (MetaNodeDiff (MetaValueUpdate update)) _) = Just (object (updateFields update))
    valueDiffToJSON _ = Nothing

valueFields :: Tree DocNodeDiff -> MetaValue -> [Pair]
valueFields valueTree (MetaMap _) = [typeField "MetaMap", "entries" .= keyDiffsToJSON (subForest valueTree)]
valueFields valueTree (MetaList _) = [typeField "MetaList", "items" .= valueDiffsToJSON (subForest valueTree)]
valueFields _ (MetaBool bool) = [typeField "MetaBool", "value" .= bool]
valueFields _ (MetaString string) = [typeField "MetaString", "value" .= string]
-- Returns only whether a rich-text value has changes: its spans and blocks would bring Pandoc's types into the
-- JSON, and the current requirements don't need them.
valueFields valueTree (MetaInlines _) = [typeField "MetaInlines", "hasChanges" .= hasChanges valueTree]
valueFields valueTree (MetaBlocks _) = [typeField "MetaBlocks", "hasChanges" .= hasChanges valueTree]

updateFields :: MetaValueDiff -> [Pair]
updateFields (MetaBoolDiff before after) = [typeField "MetaBoolDiff", "before" .= before, "after" .= after]
updateFields (MetaStringDiff before after) = [typeField "MetaStringDiff", "before" .= before, "after" .= after]

typeField :: T.Text -> Pair
typeField = ("type" .=)

nodeOpName :: NodeOp a -> T.Text
nodeOpName (Insert _) = "insert"
nodeOpName (Delete _) = "delete"
nodeOpName (Copy _) = "copy"
