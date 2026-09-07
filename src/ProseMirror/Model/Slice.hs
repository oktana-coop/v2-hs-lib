{-# LANGUAGE OverloadedStrings #-}

module ProseMirror.Model.Slice (Slice (..), emptySlice, closedSlice, isEmptySlice) where

import Data.Aeson (ToJSON, Value (Null), object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.Types (Pair)
import Data.Maybe (catMaybes)
import ProseMirror.Model.Node (Node)

data Slice = Slice {sliceContent :: [Node], openStart :: Int, openEnd :: Int} deriving (Show, Eq)

emptySlice :: Slice
emptySlice = Slice {sliceContent = [], openStart = 0, openEnd = 0}

-- A slice of whole nodes (nothing open on either side).
closedSlice :: [Node] -> Slice
closedSlice nodes = Slice {sliceContent = nodes, openStart = 0, openEnd = 0}

isEmptySlice :: Slice -> Bool
isEmptySlice = null . sliceContent

-- Returns null for an empty slice and omits zero open depths.
instance ToJSON Slice where
  toJSON s
    | isEmptySlice s = Null
    | otherwise = object $ catMaybes [Just ("content" .= sliceContent s), fieldUnlessZero "openStart" (openStart s), fieldUnlessZero "openEnd" (openEnd s)]
    where
      fieldUnlessZero :: Key -> Int -> Maybe Pair
      fieldUnlessZero name n
        | n > 0 = Just (name .= n)
        | otherwise = Nothing
