{-# LANGUAGE OverloadedStrings #-}

module ProseMirror.Transform.FromDiff (TransformedPMDoc (..), toTransformedPMDoc) where

import Control.Monad ((>=>))
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.List ((\\))
import Data.Tree (Tree, foldTree)
import qualified DocTree.Common as RichText (Mark)
import qualified DocTree.LeafTextSpans as PandocTree
import ProseMirror.Diff (PositionedDiffNode, addPositionsToDiffTree)
import ProseMirror.Model (Positioned (..), closedSlice, emptySlice, isEmptySlice)
import qualified ProseMirror.Model as PM (Block (..), BlockNode (..), InlineNode (..), Node (..), TextNode (..), isRootBlockNode, nodeSize, wrapChildrenToBlock)
import ProseMirror.Transform.Step (AttrStep (..), MarkRangeStep (..), ReplaceStep (..), Step (..))
import ProseMirror.Tree (PMTreeNode (..), leafTextSpansPandocTreeNodeToPMNode, treeMarkToPMMark)
import RichTextDiffOp (HeadingLevelDiff (..), MarkDiff (..), RichTextDiffOp (..), getDiffOp, unpackDiffOpValue)

data TransformedPMDoc = TransformedPMDoc {doc :: PM.Node, steps :: [Step]} deriving (Show, Eq)

instance ToJSON TransformedPMDoc where
  toJSON transformed = object ["doc" .= doc transformed, "steps" .= steps transformed]

toTransformedPMDoc :: Tree (RichTextDiffOp PandocTree.DocNode) -> Either String TransformedPMDoc
toTransformedPMDoc = foldTree transformSubtree . addPositionsToDiffTree >=> toDocWithSteps
  where
    toDocWithSteps :: TransformedSubtree -> Either String TransformedPMDoc
    toDocWithSteps rootSubtree = case afterNodes rootSubtree of
      [root] | PM.isRootBlockNode root -> Right TransformedPMDoc {doc = root, steps = transactionSteps}
      _ -> Left "Cannot emit steps: the diff tree does not have a single doc root."
      where
        transactionSteps = collapseDeleteInsertIntoReplace (subtreeSteps rootSubtree)

-- A subtree transformed to ProseMirror: its nodes in the "after" document, the same nodes with the deleted
-- ones still in place (a deleted ancestor is sized from them) and the steps that produce it.
data TransformedSubtree = TransformedSubtree {afterNodes :: [PM.Node], withDeletedNodes :: [PM.Node], subtreeSteps :: [Step]}

-- Sibling subtrees combine by concatenation, in document order.
instance Semigroup TransformedSubtree where
  a <> b =
    TransformedSubtree
      (afterNodes a <> afterNodes b)
      (withDeletedNodes a <> withDeletedNodes b)
      (subtreeSteps a <> subtreeSteps b)

instance Monoid TransformedSubtree where
  mempty = TransformedSubtree [] [] []

-- Transforms a diff node given its transformed children.
transformSubtree :: PositionedDiffNode -> [Either String TransformedSubtree] -> Either String TransformedSubtree
transformSubtree positionedNodeWithDiff transformedChildren = do
  childSubtrees <- sequenceA transformedChildren
  let childrenSubtree = mconcat childSubtrees
  case pmTreeNode of
    -- Wrapper nodes (a Div, the inline container) take no positions; their children speak for them.
    WrapperBlockNode -> Right childrenSubtree
    WrapperInlineNode -> Right childrenSubtree
    UnrepresentableNode -> Left "Cannot emit steps: the document contains content with no ProseMirror representation."
    PMNode nodeWithoutChildren ->
      transformPMNode
        positionedNodeWithDiff
        (wrapChildrenToNode nodeWithoutChildren (afterNodes childrenSubtree))
        (wrapChildrenToNode nodeWithoutChildren (withDeletedNodes childrenSubtree))
        (subtreeSteps childrenSubtree)
  where
    pmTreeNode = leafTextSpansPandocTreeNodeToPMNode (unpackDiffOpValue (value positionedNodeWithDiff))

-- Transforms a ProseMirror node given its diff op and positions, the node as rendered in the "after"
-- document, the node with its deleted descendants still in place, and the steps of its children.
transformPMNode :: PositionedDiffNode -> PM.Node -> PM.Node -> [Step] -> Either String TransformedSubtree
transformPMNode positionedNodeWithDiff afterNode nodeWithDeletedChildren childSteps = case getDiffOp (value positionedNodeWithDiff) of
  Copy () -> Right (nodeSubtree afterNode childSteps)
  -- Document metadata has no counterpart in the editor document, so it is treated as a copy (as the decorations do).
  UpdateMeta _ () -> Right (nodeSubtree afterNode childSteps)
  -- An inserted node's descendants are inserted too, and inserting the node inserts them all at once, so
  -- the single step here replaces the children's steps. Likewise for deletions below.
  Insert ()
    | PM.isRootBlockNode afterNode -> Left "Cannot emit steps: the document root cannot be inserted."
    | otherwise -> Right (nodeSubtree afterNode [insertStep afterNode])
  Delete ()
    | PM.isRootBlockNode nodeWithDeletedChildren -> Left "Cannot emit steps: the document root cannot be deleted."
    | otherwise -> Right (deletedNodeSubtree nodeWithDeletedChildren [deleteStep nodeWithDeletedChildren])
  UpdateMarks (MarkDiff oldMarks newMarks) () -> case afterNode of
    PM.InlineNode (PM.InlineText _) -> Right (nodeSubtree afterNode (markSteps oldMarks newMarks))
    _ -> Left "Cannot emit steps: a mark update on a node that is not text."
  UpdateHeadingLevel (HeadingLevelDiff _ level) () -> case afterNode of
    PM.BlockNode (PM.PMBlock (PM.Heading _) _) -> Right (nodeSubtree afterNode (headingLevelStep level : childSteps))
    _ -> Left "Cannot emit steps: a heading level update on a node that is not a heading."
  where
    start = startPos positionedNodeWithDiff
    end = endPos positionedNodeWithDiff

    -- A node present in the "after" document, with the steps of its subtree.
    nodeSubtree :: PM.Node -> [Step] -> TransformedSubtree
    nodeSubtree node stepsOfSubtree = TransformedSubtree [node] [node] stepsOfSubtree

    -- A node absent from the "after" document but kept for sizing its deletion, with the steps of its subtree.
    deletedNodeSubtree :: PM.Node -> [Step] -> TransformedSubtree
    deletedNodeSubtree node stepsOfSubtree = TransformedSubtree [] [node] stepsOfSubtree

    insertStep :: PM.Node -> Step
    insertStep node = ReplaceStep PMReplaceStep {replaceFrom = start, replaceTo = start, slice = closedSlice [node], structure = False}

    deleteStep :: PM.Node -> Step
    deleteStep node = ReplaceStep PMReplaceStep {replaceFrom = start, replaceTo = start + PM.nodeSize node, slice = emptySlice, structure = False}

    -- One step per mark that is removed and one per mark that is added, over the node's range.
    markSteps :: [RichText.Mark] -> [RichText.Mark] -> [Step]
    markSteps oldMarks newMarks = map (RemoveMarkStep . markRangeStep) removedMarks <> map (AddMarkStep . markRangeStep) addedMarks
      where
        removedMarks = oldMarks \\ newMarks
        addedMarks = newMarks \\ oldMarks

    markRangeStep :: RichText.Mark -> MarkRangeStep
    markRangeStep m = PMMarkRangeStep {markFrom = start, markTo = end, mark = treeMarkToPMMark m}

    headingLevelStep :: Int -> Step
    headingLevelStep level = AttrStep PMAttrStep {attrPos = start, attrName = "level", attrValue = toJSON level}

-- `wrapChildrenToBlock` lifted to any node (inline nodes have no children), joining adjacent text on the way.
wrapChildrenToNode :: PM.Node -> [PM.Node] -> PM.Node
wrapChildrenToNode (PM.BlockNode blockNode) children = PM.BlockNode (PM.wrapChildrenToBlock blockNode (joinAdjacentText children))
wrapChildrenToNode inlineNode _ = inlineNode

-- The diff splits text at op boundaries, but ProseMirror joins adjacent text nodes with the same marks
-- when applying a replace.
joinAdjacentText :: [PM.Node] -> [PM.Node]
joinAdjacentText = foldr joinOrPrepend []
  where
    joinOrPrepend :: PM.Node -> [PM.Node] -> [PM.Node]
    joinOrPrepend (PM.InlineNode (PM.InlineText textNode1)) (PM.InlineNode (PM.InlineText textNode2) : rest)
      | PM.marks textNode1 == PM.marks textNode2 =
          PM.InlineNode (PM.InlineText PM.PMText {PM.text = PM.text textNode1 <> PM.text textNode2, PM.marks = PM.marks textNode1}) : rest
    joinOrPrepend node rest = node : rest

collapseDeleteInsertIntoReplace :: [Step] -> [Step]
collapseDeleteInsertIntoReplace (ReplaceStep replace1 : ReplaceStep replace2 : rest)
  | isDeletion replace1 && insertsAtStartOf replace1 replace2 =
      collapseDeleteInsertIntoReplace (ReplaceStep replace1 {slice = slice replace2} : rest)
  where
    isDeletion :: ReplaceStep -> Bool
    isDeletion = isEmptySlice . slice

    insertsAtStartOf :: ReplaceStep -> ReplaceStep -> Bool
    insertsAtStartOf deletion insertion = replaceFrom insertion == replaceFrom deletion && replaceTo insertion == replaceFrom insertion
collapseDeleteInsertIntoReplace (step : rest) = step : collapseDeleteInsertIntoReplace rest
collapseDeleteInsertIntoReplace [] = []
