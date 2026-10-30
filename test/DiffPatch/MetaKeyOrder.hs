module DiffPatch.MetaKeyOrder (tests) where

import Conversion (readDocuments)
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Tree (Tree (..))
import Diffing (diffForProseMirror)
import qualified DocTree.LeafTextSpans as PandocTree
import Format (Format (Markdown))
import RichTextDiff (defaultDiffOptions)
import RichTextDiffOp (RichTextDiffOp, RichTextDiffOpType (CopyType), getDiffOpType, unpackDiffOpValue)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

tests :: IO TestTree
tests = testSpec "Metadata Key Order in Diffs" spec

-- The documents of the diff & patch golden cases, whose steps the goldens pin.
casesDir :: FilePath
casesDir = "test/DiffPatch/ProseMirror/meta"

caseDiff :: FilePath -> IO (Tree (RichTextDiffOp PandocTree.DocNode))
caseDiff caseName = do
  doc1 <- TIO.readFile (casesDir </> caseName </> "doc1.md")
  doc2 <- TIO.readFile (casesDir </> caseName </> "doc2.md")
  eitherDocs <- readDocuments Markdown Markdown (T.unpack doc1) (T.unpack doc2)
  either (fail . show) (pure . snd) (eitherDocs >>= diffForProseMirror defaultDiffOptions)

-- The diff of a case's metadata.
caseMetadataDiff :: FilePath -> IO (Tree (RichTextDiffOp PandocTree.DocNode))
caseMetadataDiff caseName = do
  diff <- caseDiff caseName
  maybe (fail "Found no metadata in the diff") pure (findMetadataDiff diff)

findMetadataDiff :: Tree (RichTextDiffOp PandocTree.DocNode) -> Maybe (Tree (RichTextDiffOp PandocTree.DocNode))
findMetadataDiff diff = find isMetadata (subForest diff)
  where
    isMetadata subtree = unpackDiffOpValue (rootLabel subtree) == PandocTree.Metadata

-- Whether any node of a diff has an op other than `Copy`: a `Copy` node can hold changes in its subtree.
hasChanges :: Tree (RichTextDiffOp a) -> Bool
hasChanges = any (\diffOp -> getDiffOpType diffOp /= CopyType)

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
