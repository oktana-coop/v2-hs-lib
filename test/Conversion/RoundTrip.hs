module Conversion.RoundTrip (tests) where

import Conversion (Format (Markdown, ProseMirror))
import Conversion.Utils (toTextFormat)
import qualified Data.Text.IO as TIO
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (testSpec)

tests :: IO TestTree
tests = do
  hspecTests <- testSpec "hspec" spec
  return $ testGroup "Round-Trip Conversion" [hspecTests]

spec :: Spec
spec = do
  describe "Markdown -> ProseMirror -> Markdown" $ do
    let inputFiles = map buildMdFilePathForCase ["headings-and-paragraphs"]
    -- roundTripTestForInputFile returns a monadic action (Spec is a monad)
    -- mapM_ combines these `Spec` actions into a single `Spec`.
    -- We use mapM_ because we don't care about the returned value, just the effect of these tests.
    mapM_ roundTripTestForInputFile inputFiles

buildMdFilePathForCase :: FilePath -> FilePath
buildMdFilePathForCase caseSubFolder = baseDir </> "doc.md"
  where
    baseDir = "test/Conversion/MdToPm" </> caseSubFolder

roundTripTestForInputFile :: FilePath -> Spec
roundTripTestForInputFile filePath = it filePath $ do
  inputText <- TIO.readFile filePath
  intermediaryFormatText <- toTextFormat Markdown ProseMirror inputText
  outputText <- toTextFormat ProseMirror Markdown intermediaryFormatText

  inputText `shouldBe` outputText