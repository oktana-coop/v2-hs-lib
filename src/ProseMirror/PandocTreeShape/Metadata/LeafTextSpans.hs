-- TODO: Leave out the contents of the metadata's notes too (see `ProseMirror.PandocTreeShape.Metadata.GroupedInlines`).
module ProseMirror.PandocTreeShape.Metadata.LeafTextSpans (withoutMetadataSubtree) where

import Data.Tree (Tree (..))
import qualified DocTree.LeafTextSpans as LeafTextSpansTree

-- The tree without its metadata subtree, given how to get the node from a label (e.g. a diff node).
withoutMetadataSubtree :: (a -> LeafTextSpansTree.DocNode) -> Tree a -> Tree a
withoutMetadataSubtree extract (Node root childTrees) = Node root (filter (not . isMetadataTree) childTrees)
  where
    isMetadataTree tree = extract (rootLabel tree) == LeafTextSpansTree.Metadata
