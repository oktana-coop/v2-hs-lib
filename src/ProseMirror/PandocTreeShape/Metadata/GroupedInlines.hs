-- The metadata has no nodes in ProseMirror (the doc's attribute holds it), so a tree reconciled with its model has no
-- metadata subtree.
-- TODO: Leave out the contents of the metadata's notes too. pandoc-tree puts them in the body, after its blocks, so
-- the editor shows them as notes that nothing in the text refers to, and edits to them are lost on save (the
-- attribute keeps its own copy).
module ProseMirror.PandocTreeShape.Metadata.GroupedInlines (withoutMetadataSubtree) where

import Data.Tree (Tree (..))
import qualified DocTree.GroupedInlines as GroupedInlinesTree

withoutMetadataSubtree :: Tree GroupedInlinesTree.DocNode -> Tree GroupedInlinesTree.DocNode
withoutMetadataSubtree (Node root childTrees) = Node root (filter (not . isMetadataTree) childTrees)
  where
    isMetadataTree :: Tree GroupedInlinesTree.DocNode -> Bool
    isMetadataTree tree = rootLabel tree == GroupedInlinesTree.Metadata
