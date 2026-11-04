-- The metadata has no nodes in ProseMirror (the doc's attribute holds it), so a tree reconciled with its model has no
-- metadata subtree.
module ProseMirror.PandocTreeShape.Metadata.GroupedInlines (withoutMetadataSubtree) where

import Data.Tree (Tree (..))
import qualified DocTree.GroupedInlines as GroupedInlinesTree

withoutMetadataSubtree :: Tree GroupedInlinesTree.DocNode -> Tree GroupedInlinesTree.DocNode
withoutMetadataSubtree (Node root childTrees) = Node root (filter (not . isMetadataTree) childTrees)
  where
    isMetadataTree :: Tree GroupedInlinesTree.DocNode -> Bool
    isMetadataTree tree = rootLabel tree == GroupedInlinesTree.Metadata
