{-# LANGUAGE OverloadedStrings #-}

module ProseMirror.Transform.FromDiff (TransformedPMDoc (..), toTransformedPMDoc) where

import Control.Monad ((>=>))
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.List ((\\))
import Data.Tree (Tree, foldTree)
import qualified DocTree.Common as RichText (Mark)
import NodeDiff.Common (DiffOpType (..), HeadingLevelDiff (..), InlineContentDiff (..), MarkDiff (..))
import NodeDiff.LeafTextSpans (DocNodeDiff (..), RichTextNodeDiff (..), toDiffOpType, toDocNode)
import ProseMirror.Diff (PositionedDiffNode, addPositionsToDiffTree)
import ProseMirror.Metadata (addDocFrontMatter)
import ProseMirror.Model (Positioned (..), closedSlice, emptySlice, isEmptySlice)
import qualified ProseMirror.Model as PM (Block (..), BlockNode (..), FrontMatter, InlineNode (..), Node (..), TextNode (..), nodeSize, wrapChildrenToBlock)
import ProseMirror.Transform.Step (AttrStep (..), DocAttrStep (..), MarkRangeStep (..), ReplaceStep (..), Step (..))
import ProseMirror.Tree (PMTreeNode (..), leafTextSpansPandocTreeNodeToPMNode, treeMarkToPMMark)

data TransformedPMDoc = TransformedPMDoc {doc :: PM.Node, steps :: [Step]} deriving (Show, Eq)

instance ToJSON TransformedPMDoc where
  toJSON transformed = object ["doc" .= doc transformed, "steps" .= steps transformed]

-- With `updatesMeta`, the steps also update the editor's metadata. The doc's attribute holds the given front matter.
toTransformedPMDoc :: Bool -> Maybe PM.FrontMatter -> Tree DocNodeDiff -> Either String TransformedPMDoc
toTransformedPMDoc updatesMeta docFrontMatter = foldTree transformSubtree . addPositionsToDiffTree >=> toDocWithSteps
  where
    toDocWithSteps :: TransformedSubtree -> Either String TransformedPMDoc
    toDocWithSteps rootSubtree = case afterNodesWithFrontMatter of
      [root@(PM.BlockNode (PM.PMBlock (PM.Doc frontMatter) _))] -> Right TransformedPMDoc {doc = root, steps = transactionSteps frontMatter}
      _ -> Left "Cannot emit steps: the diff tree does not have a single doc root."
      where
        afterNodesWithFrontMatter :: [PM.Node]
        afterNodesWithFrontMatter = map (addDocFrontMatter docFrontMatter) (afterNodes rootSubtree)

        transactionSteps :: Maybe PM.FrontMatter -> [Step]
        transactionSteps frontMatter = collapseDeleteInsertIntoReplace (docMetaSteps frontMatter <> subtreeSteps rootSubtree)

        -- Document metadata is an attribute of the editor's doc node, set by a step of its own.
        docMetaSteps :: Maybe PM.FrontMatter -> [Step]
        docMetaSteps frontMatter =
          if updatesMeta
            then [DocAttrStep PMDocAttrStep {docAttrName = "frontMatter", docAttrValue = toJSON frontMatter}]
            else []

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
      Right $
        transformPMNode
          positionedNodeWithDiff
          (wrapChildrenToNode nodeWithoutChildren (afterNodes childrenSubtree))
          (wrapChildrenToNode nodeWithoutChildren (withDeletedNodes childrenSubtree))
          (subtreeSteps childrenSubtree)
  where
    pmTreeNode = leafTextSpansPandocTreeNodeToPMNode (toDocNode (value positionedNodeWithDiff))

-- Transforms a ProseMirror node given its diff op and positions, the node as rendered in the "after"
-- document, the node with its deleted descendants still in place, and the steps of its children.
transformPMNode :: PositionedDiffNode -> PM.Node -> PM.Node -> [Step] -> TransformedSubtree
transformPMNode positionedNodeWithDiff afterNode nodeWithDeletedChildren childSteps = case value positionedNodeWithDiff of
  RichTextNodeDiff (InlineContentDiff (MarksUpdate (MarkDiff oldMarks newMarks) _)) -> nodeSubtree afterNode (markSteps oldMarks newMarks)
  RichTextNodeDiff (HeadingLevelUpdate (HeadingLevelDiff _ level) _) -> nodeSubtree afterNode (headingLevelStep level : childSteps)
  nodeWithDiff -> case toDiffOpType nodeWithDiff of
    -- An inserted node's descendants are inserted too, and inserting the node inserts them all at once, so
    -- the single step here replaces the children's steps. Likewise for deletions below.
    InsertType -> nodeSubtree afterNode [insertStep afterNode]
    DeleteType -> deletedNodeSubtree nodeWithDeletedChildren [deleteStep nodeWithDeletedChildren]
    -- Copies. The rich text's updates are matched above, and the metadata's is left out before the positions.
    _ -> nodeSubtree afterNode childSteps
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
