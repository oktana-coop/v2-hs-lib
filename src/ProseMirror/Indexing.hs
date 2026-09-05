{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ProseMirror.Indexing (PMPosition, Positioned (..), addNodePositions, addNodePositionsRenderedBy) where

import Control.Monad.State (State, evalState, get, modify)
import Data.Tree (Tree (..))
import qualified ProseMirror.Model as PM (isLeafNode, isRootBlockNode, nodeSize)
import ProseMirror.Tree (PMTree, PMTreeNode (..))

-- A document-wide position in ProseMirror's integer-based indexing scheme.
-- https://prosemirror.net/docs/guide/#doc.indexing
type PMPosition = Int

-- A tree node together with the positions right before and right after it.
data Positioned a = Positioned {value :: a, startPos :: PMPosition, endPos :: PMPosition} deriving (Show, Functor)

-- Adds positions to a plain ProseMirror tree: every node renders to itself and is present.
addNodePositions :: PMTree -> Tree (Positioned PMTreeNode)
addNodePositions = addNodePositionsRenderedBy Just

-- Adds positions to the nodes of a tree whose nodes render to ProseMirror tree nodes.
addNodePositionsRenderedBy :: forall a. (a -> Maybe PMTreeNode) -> Tree a -> Tree (Positioned a)
addNodePositionsRenderedBy renderNode tree = evalState (walk tree) 0
  where
    walk :: Tree a -> State PMPosition (Tree (Positioned a))
    walk (Node node subTrees) = do
      start <- get
      children <- case renderNode node of
        -- A leaf has no content, so its whole size is counted at once (its subtrees are empty).
        Just (PMNode leaf) | PM.isLeafNode leaf -> modify (+ PM.nodeSize leaf) *> mapM walk subTrees
        -- A non-leaf node's content is delimited by an open and a close token, one position each.
        Just (PMNode nonLeaf) | not (PM.isRootBlockNode nonLeaf) -> countToken *> mapM walk subTrees <* countToken
        -- The rest (the root, wrapper nodes, nodes that don't render) take no positions of their own; just walk the subtrees.
        _ -> mapM walk subTrees
      end <- get
      pure $ Node (Positioned {value = node, startPos = start, endPos = end}) children

    countToken :: State PMPosition ()
    countToken = modify (+ 1)
