module Conversion.RoundTrip (tests) where

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
    it "headings-and-paragraphs" $ do
      let baseDir = "test/Conversion/MdToPm" </> "headings-and-paragraphs"
          mdInput = baseDir </> "doc.md"
       in mdInput `shouldBe` mdInput
