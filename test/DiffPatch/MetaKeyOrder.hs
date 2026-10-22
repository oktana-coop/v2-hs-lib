module DiffPatch.MetaKeyOrder (tests) where

import Conversion (readDocuments)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Tree (Tree (..))
import Diffing (diffForProseMirror)
import qualified DocTree.LeafTextSpans as PandocTree
import Format (Format (Markdown))
import RichTextDiff (defaultDiffOptions)
import RichTextDiffOp (RichTextDiffOp, RichTextDiffOpType (..), getDiffOpType)
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
  either (fail . show) (pure . diffForProseMirror defaultDiffOptions) eitherDocs

rootDiffOpType :: Tree (RichTextDiffOp PandocTree.DocNode) -> RichTextDiffOpType
rootDiffOpType = getDiffOpType . rootLabel

spec :: Spec
spec =
  describe "Markdown diff" $ do
    it "has no metadata change for a reorder alone" $ do
      diff <- caseDiff "reorder-metadata-keys"
      rootDiffOpType diff `shouldBe` CopyType

    it "diffs a reorder with a body change as the body change alone" $ do
      reorderDiff <- caseDiff "reorder-metadata-keys-and-change-body"
      bodyChangeDiff <- caseDiff "change-body"
      reorderDiff `shouldBe` bodyChangeDiff

    it "has a metadata change for a changed value" $ do
      diff <- caseDiff "change-metadata-value"
      rootDiffOpType diff `shouldBe` UpdateMetaType
