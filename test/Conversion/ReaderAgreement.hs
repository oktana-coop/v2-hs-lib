module Conversion.ReaderAgreement (tests) where

import Control.Monad (filterM)
import Conversion (pandocReaderOptions, readFrom)
import Conversion.Utils (toTextFormat)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Tree (Tree)
import DocTree.GroupedInlines (DocNode, toTree)
import Format (Format (Markdown, ProseMirror))
import ProseMirror.PandocTreeShape (dropDetailsLostInProseMirror, reconcileWithProseMirrorModel)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeFileName, (</>))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Pandoc (Pandoc, handleError)
import Text.Pandoc.Class (runIO)

tests :: IO TestTree
tests = do
  markdownFiles <- listMarkdownFiles "test/Conversion/MdToPm" "doc.md"
  testSpec "Reader Agreement" (spec markdownFiles)

-- Every input is a Markdown document of the conversion corpus. The test converts it to ProseMirror JSON,
-- reads both texts back with their respective readers, and expects the same document tree once both are
-- reduced to what ProseMirror can represent.
spec :: [FilePath] -> Spec
spec markdownFiles =
  describe "Markdown and ProseMirror readers agree on the document tree" $
    mapM_ agreementTestForMarkdownFile markdownFiles

-- The files with the given name anywhere under the directory, found on disk so that every document in the
-- corpus is covered whatever the golden suite lists.
listMarkdownFiles :: FilePath -> FilePath -> IO [FilePath]
listMarkdownFiles directory fileName = do
  paths <- map (directory </>) <$> listDirectory directory
  subdirectories <- filterM doesDirectoryExist paths
  nestedFiles <- concat <$> mapM (`listMarkdownFiles` fileName) subdirectories
  pure (filter ((== fileName) . takeFileName) paths ++ nestedFiles)

agreementTestForMarkdownFile :: FilePath -> Spec
agreementTestForMarkdownFile markdownFile = it markdownFile $ do
  markdown <- TIO.readFile markdownFile
  proseMirrorJson <- toTextFormat Markdown ProseMirror markdown
  treeFromMarkdown <- readAsTree Markdown markdown
  treeFromProseMirror <- readAsTree ProseMirror proseMirrorJson
  treeFromProseMirror `shouldBe` treeFromMarkdown

readAsTree :: Format -> T.Text -> IO (Tree DocNode)
readAsTree format text = do
  doc <- runIO (readFrom format pandocReaderOptions text) >>= handleError
  pure (toTree (normalize doc))
  where
    normalize :: Pandoc -> Pandoc
    normalize = dropDetailsLostInProseMirror . reconcileWithProseMirrorModel
