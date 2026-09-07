{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ProseMirror.Model.Indexing (PMPosition, Positioned (..), addNodePositionsRenderedBy) where

import Control.Monad.State (State, evalState, get, modify)
import Data.Tree (Tree (..))
import qualified ProseMirror.Model.Node as PM (Node, isLeafNode, isRootBlockNode, nodeSize)

type PMPosition = Int

-- A PM node together with the positions right before and right after it.
data Positioned a = Positioned {value :: a, startPos :: PMPosition, endPos :: PMPosition} deriving (Show, Functor)

-- Adds positions to the nodes of a tree whose nodes render to ProseMirror nodes. `renderNode` gives the
-- ProseMirror node a node renders to, or `Nothing` when it renders to none (deleted content in a diff,
-- wrapper nodes with no ProseMirror counterpart), in which case it takes no positions of its own.
addNodePositionsRenderedBy :: forall a. (a -> Maybe PM.Node) -> Tree a -> Tree (Positioned a)
addNodePositionsRenderedBy renderNode tree = evalState (walk tree) 0
  where
    walk :: Tree a -> State PMPosition (Tree (Positioned a))
    walk (Node node subTrees) = do
      start <- get
      children <- case renderNode node of
        -- A leaf has no content, so its whole size is counted at once (its subtrees are empty).
        Just leaf | PM.isLeafNode leaf -> modify (+ PM.nodeSize leaf) *> mapM walk subTrees
        -- A non-leaf node's content is delimited by an open and a close token, one position each.
        Just nonLeaf | not (PM.isRootBlockNode nonLeaf) -> countToken *> mapM walk subTrees <* countToken
        -- The rest (the root, nodes that don't render) take no positions of their own; just walk the subtrees.
        _ -> mapM walk subTrees
      end <- get
      pure $ Node (Positioned {value = node, startPos = start, endPos = end}) children

    countToken :: State PMPosition ()
    countToken = modify (+ 1)
