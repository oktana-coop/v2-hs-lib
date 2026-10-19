{-# LANGUAGE OverloadedStrings #-}

module Conversion.MetaKeyOrder (tests) where

import Conversion.Utils (toTextFormat)
import qualified Data.Text as T
import Format (Format (Json, Markdown, ProseMirror))
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotSatisfy)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

tests :: IO TestTree
tests = testSpec "Metadata Key Order" spec

markdownThroughProseMirror :: T.Text -> IO T.Text
markdownThroughProseMirror markdown = toTextFormat Markdown ProseMirror markdown >>= toTextFormat ProseMirror Markdown

spec :: Spec
spec = do
  describe "Markdown -> ProseMirror -> Markdown" $
    it "reads the key order of front matter with Windows line endings" $ do
      output <- markdownThroughProseMirror "---\r\nzeta: 1\r\nalpha: 2\r\n---\r\n\r\nText.\r\n"
      output `shouldBe` T.unlines ["---", "zeta: 1", "alpha: 2", "---", "", "Text."]

  describe "Markdown -> other formats" $
    it "does not pass the key order to other writers" $ do
      output <- toTextFormat Markdown Json (T.unlines ["---", "zeta: 1", "alpha: 2", "---", "", "Text."])
      output `shouldNotSatisfy` T.isInfixOf "tmp-key-outline_"
