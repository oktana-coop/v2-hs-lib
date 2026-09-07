{-# LANGUAGE OverloadedStrings #-}

module ProseMirror.Transform.Step (Step (..), ReplaceStep (..), MarkRangeStep (..), AttrStep (..)) where

import Data.Aeson (ToJSON, Value, object, toJSON, (.=))
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import ProseMirror.Model (PMPosition, Slice, isEmptySlice)
import qualified ProseMirror.Model as PM (Mark)

data ReplaceStep = PMReplaceStep {replaceFrom :: PMPosition, replaceTo :: PMPosition, slice :: Slice, structure :: Bool} deriving (Show, Eq)

data MarkRangeStep = PMMarkRangeStep {markFrom :: PMPosition, markTo :: PMPosition, mark :: PM.Mark} deriving (Show, Eq)

data AttrStep = PMAttrStep {attrPos :: PMPosition, attrName :: T.Text, attrValue :: Value} deriving (Show, Eq)

data Step = ReplaceStep ReplaceStep | AddMarkStep MarkRangeStep | RemoveMarkStep MarkRangeStep | AttrStep AttrStep deriving (Show, Eq)

instance ToJSON Step where
  toJSON (ReplaceStep step) =
    object $
      catMaybes
        [ Just ("stepType" .= T.pack "replace"),
          Just ("from" .= replaceFrom step),
          Just ("to" .= replaceTo step),
          if isEmptySlice (slice step) then Nothing else Just ("slice" .= slice step),
          if structure step then Just ("structure" .= True) else Nothing
        ]
  toJSON (AddMarkStep step) = markStepToJSON "addMark" step
  toJSON (RemoveMarkStep step) = markStepToJSON "removeMark" step
  toJSON (AttrStep step) = object ["stepType" .= T.pack "attr", "pos" .= attrPos step, "attr" .= attrName step, "value" .= attrValue step]

markStepToJSON :: T.Text -> MarkRangeStep -> Value
markStepToJSON stepType step = object ["stepType" .= stepType, "mark" .= mark step, "from" .= markFrom step, "to" .= markTo step]
