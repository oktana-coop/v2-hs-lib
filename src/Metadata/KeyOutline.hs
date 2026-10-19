{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Metadata.KeyOutline (KeyOutline (..), mapEntriesInOutlineOrder, listItemsWithOutlines, addKeyOutline, takeKeyOutline, stripTempMetaOutlineKey) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrictText)
import Data.Aeson.Text (encodeToLazyText)
import Data.Function (on)
import Data.List (nubBy, sortOn)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import GHC.Generics (Generic)
import Text.Pandoc.Definition (Meta (..), MetaValue (..), isNullMeta)

-- The keys of the metadata in their order, in the shape of the metadata: a map has its keys in order, each with
-- the outline of its value, and a list has the outline of each item.
data KeyOutline
  = MapOutline [(T.Text, KeyOutline)]
  | ListOutline [KeyOutline]
  | -- A scalar value, or one the order of which is unknown.
    NoOutline
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- The entries of a map in the order of an outline.
mapEntriesInOutlineOrder :: KeyOutline -> M.Map T.Text a -> [(T.Text, a, KeyOutline)]
mapEntriesInOutlineOrder outline m = entriesInOutline <> entriesNotInOutline
  where
    outlineEntries = withoutRepeatedKeys (mapEntries outline)
    outlineKeys = Set.fromList (map fst outlineEntries)

    entriesInOutline = mapMaybe (findEntryInMap m) outlineEntries
      where
        -- A key of the outline the map does not have, e.g. one Pandoc ignores, is left out.
        findEntryInMap :: M.Map T.Text b -> (T.Text, KeyOutline) -> Maybe (T.Text, b, KeyOutline)
        findEntryInMap entryMap (key, keyOutline) = case M.lookup key entryMap of
          Just value -> Just (key, value, keyOutline)
          Nothing -> Nothing

    -- The keys of the map the outline does not have, e.g. from a YAML block further down, come after.
    entriesNotInOutline = map withNoOutline (sortAlphabeticallyIgnoringCase unsortedEntries)
      where
        unsortedEntries = M.toList (M.withoutKeys m outlineKeys)

        -- Pandoc's writers sort metadata keys like this.
        sortAlphabeticallyIgnoringCase :: [(T.Text, b)] -> [(T.Text, b)]
        sortAlphabeticallyIgnoringCase = sortOn (T.toCaseFold . fst)

        withNoOutline :: (T.Text, b) -> (T.Text, b, KeyOutline)
        withNoOutline (key, value) = (key, value, NoOutline)

    mapEntries :: KeyOutline -> [(T.Text, KeyOutline)]
    mapEntries (MapOutline entries) = entries
    mapEntries _ = []

    -- A key that appears more than once keeps the place of its first appearance.
    withoutRepeatedKeys :: [(T.Text, KeyOutline)] -> [(T.Text, KeyOutline)]
    withoutRepeatedKeys = nubBy ((==) `on` fst)

-- The items of a list, each with its outline. Items past the ones the outline has have none.
listItemsWithOutlines :: KeyOutline -> [a] -> [(a, KeyOutline)]
listItemsWithOutlines (ListOutline outlines) items = zip items (outlines <> repeat NoOutline)
listItemsWithOutlines _ items = map (\item -> (item, NoOutline)) items

-- The outline travels inside the metadata, under a temporary key of its own, from a reader to a writer.
-- Pandoc drops YAML keys that end with an underscore, so no front matter can have this key.
tempMetaOutlineKey :: T.Text
tempMetaOutlineKey = "tmp-key-outline_"

addKeyOutline :: KeyOutline -> Meta -> Meta
addKeyOutline outline meta
  | isNullMeta metaWithoutOutline = metaWithoutOutline
  | otherwise = Meta (M.insert tempMetaOutlineKey (MetaString (outlineToJson normalizedOutline)) (unMeta metaWithoutOutline))
  where
    -- An outline the metadata already has is replaced, not read as a key of it.
    metaWithoutOutline = stripTempMetaOutlineKey meta

    normalizedOutline = normalizeKeyOutline outline metaWithoutOutline

-- An outline read from the input can have keys the metadata does not, e.g. one Pandoc ignores, and miss
-- keys it has, e.g. from a YAML block further down. The normalized outline has exactly the keys of the
-- metadata, in the order of `mapEntriesInOutlineOrder`, so the same metadata has the same outline whatever
-- format it was read from.
normalizeKeyOutline :: KeyOutline -> Meta -> KeyOutline
normalizeKeyOutline inputOutline meta = normalizeMapOutline inputOutline (unMeta meta)
  where
    normalizeMapOutline :: KeyOutline -> M.Map T.Text MetaValue -> KeyOutline
    normalizeMapOutline outline metaMap = MapOutline (map normalizeEntryOutline (mapEntriesInOutlineOrder outline metaMap))

    normalizeEntryOutline :: (T.Text, MetaValue, KeyOutline) -> (T.Text, KeyOutline)
    normalizeEntryOutline (key, value, valueOutline) = (key, normalizeValueOutline valueOutline value)

    normalizeValueOutline :: KeyOutline -> MetaValue -> KeyOutline
    normalizeValueOutline outline (MetaMap metaMap) = normalizeMapOutline outline metaMap
    normalizeValueOutline outline (MetaList items) = ListOutline (map normalizeItemOutline (listItemsWithOutlines outline items))
    normalizeValueOutline _ _ = NoOutline

    normalizeItemOutline :: (MetaValue, KeyOutline) -> KeyOutline
    normalizeItemOutline (item, itemOutline) = normalizeValueOutline itemOutline item

-- The outline that travels inside the metadata, and the metadata without it.
takeKeyOutline :: Meta -> (KeyOutline, Meta)
takeKeyOutline (Meta metaMap) = (outline, Meta (M.delete tempMetaOutlineKey metaMap))
  where
    outline = case M.lookup tempMetaOutlineKey metaMap of
      Just (MetaString json) -> outlineFromJson json
      _ -> NoOutline

stripTempMetaOutlineKey :: Meta -> Meta
stripTempMetaOutlineKey = snd . takeKeyOutline

outlineToJson :: KeyOutline -> T.Text
outlineToJson = LT.toStrict . encodeToLazyText

-- Text that is not the JSON of an outline gives no outline.
outlineFromJson :: T.Text -> KeyOutline
outlineFromJson = either (const NoOutline) id . eitherDecodeStrictText
