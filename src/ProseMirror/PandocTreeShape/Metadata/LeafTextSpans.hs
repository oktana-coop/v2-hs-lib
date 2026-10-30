{-# LANGUAGE RankNTypes #-}

-- TODO: Leave out the contents of the metadata's notes too (see `ProseMirror.PandocTreeShape.Metadata.GroupedInlines`).
module ProseMirror.PandocTreeShape.Metadata.LeafTextSpans (withoutMetadataSubtree) where

import Data.Tree (Tree (..))
import qualified DocTree.LeafTextSpans as LeafTextSpansTree

-- The tree without its metadata subtree, given how to extract the node from its wrapper (e.g. a diff op).
withoutMetadataSubtree :: (forall x. w x -> x) -> Tree (w LeafTextSpansTree.DocNode) -> Tree (w LeafTextSpansTree.DocNode)
withoutMetadataSubtree extract (Node root childTrees) = Node root (filter (not . isMetadataTree) childTrees)
  where
    isMetadataTree tree = extract (rootLabel tree) == LeafTextSpansTree.Metadata
