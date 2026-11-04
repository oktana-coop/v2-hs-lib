{-# LANGUAGE OverloadedStrings #-}

module DiffPatch.MetaKeyOrder (tests) where

import Conversion (readDocuments)
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Tree (Tree (..))
import Diffing (diffForProseMirror)
import Format (Format (Markdown))
import NodeDiff.Common (DiffOpType (CopyType), MetaNodeDiff (MetaKeyDiff), toNode)
import NodeDiff.LeafTextSpans (DocNodeDiff (MetaNodeDiff, MetadataDiff), toDiffOpType)
import RichTextDiff (defaultDiffOptions)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

tests :: IO TestTree
tests = testSpec "Metadata Key Order in Diffs" spec

-- The documents of the diff & patch golden cases, whose steps the goldens pin.
casesDir :: FilePath
casesDir = "test/DiffPatch/ProseMirror/meta"

caseDiff :: FilePath -> IO (Tree DocNodeDiff)
caseDiff caseName = do
  doc1 <- TIO.readFile (casesDir </> caseName </> "doc1.md")
  doc2 <- TIO.readFile (casesDir </> caseName </> "doc2.md")
  eitherDocs <- readDocuments Markdown Markdown (T.unpack doc1) (T.unpack doc2)
  either (fail . show) (pure . snd) (eitherDocs >>= diffForProseMirror defaultDiffOptions)

-- The diff of a case's metadata.
caseMetadataDiff :: FilePath -> IO (Tree DocNodeDiff)
caseMetadataDiff caseName = do
  diff <- caseDiff caseName
  maybe (fail "Found no metadata in the diff") pure (findMetadataDiff diff)

findMetadataDiff :: Tree DocNodeDiff -> Maybe (Tree DocNodeDiff)
findMetadataDiff diff = find isMetadata (subForest diff)
  where
    isMetadata subtree = rootLabel subtree == MetadataDiff

-- Whether any node of a diff has an op other than `Copy`: a `Copy` node can hold changes in its subtree.
hasChanges :: Tree DocNodeDiff -> Bool
hasChanges = any (\nodeDiff -> toDiffOpType nodeDiff /= CopyType)

-- The keys of a metadata diff whose subtree has changes.
changedKeys :: Tree DocNodeDiff -> [T.Text]
changedKeys metadataDiff = [toNode keyOp | keyDiff@(Node (MetaNodeDiff (MetaKeyDiff keyOp)) _) <- subForest metadataDiff, hasChanges keyDiff]

spec :: Spec
spec =
  describe "Markdown diff" $ do
    it "has no metadata change for a reorder alone" $ do
      metadataDiff <- caseMetadataDiff "reorder-metadata-keys"
      hasChanges metadataDiff `shouldBe` False

    it "diffs a reorder with a body change as the body change alone" $ do
      reorderDiff <- caseDiff "reorder-metadata-keys-and-change-body"
      bodyChangeDiff <- caseDiff "change-body"
      reorderDiff `shouldBe` bodyChangeDiff

    it "has a metadata change for a changed value" $ do
      metadataDiff <- caseMetadataDiff "change-metadata-value"
      hasChanges metadataDiff `shouldBe` True

    it "has changes only in the changed key's subtree for a reorder with a changed value" $ do
      metadataDiff <- caseMetadataDiff "reorder-metadata-keys-and-change-value"
      changedKeys metadataDiff `shouldBe` ["status"]
