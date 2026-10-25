{-# LANGUAGE OverloadedStrings #-}

module DiffPatch.Errors (tests) where

import Data.List.NonEmpty (fromList)
import Diffing (proseMirrorDiff)
import Error (Error (..), UnrepresentableContent (..))
import Format (Format (Markdown, ProseMirror))
import Patching (proseMirrorSteps)
import Test.Hspec (Spec, describe, it, shouldReturn)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Utils (errorsOf)

tests :: IO TestTree
tests = testSpec "Diff and patch errors" spec

proseMirrorHello :: String
proseMirrorHello = "{\"type\":\"doc\",\"content\":[{\"type\":\"paragraph\",\"content\":[{\"type\":\"text\",\"text\":\"Hello\"}]}]}"

spec :: Spec
spec = do
  describe "proseMirrorDiff" $ do
    it "gives the read errors of both documents" $
      (proseMirrorDiff ProseMirror "{" "[" >>= errorsOf)
        `shouldReturn` [ ReadError ProseMirror "Unexpected end-of-input, expecting record key literal or }",
                         ReadError ProseMirror "Unexpected end-of-input, expecting JSON value or ]"
                       ]

    it "fails with an unrepresentable content error for blocks ProseMirror can't represent" $
      (proseMirrorDiff Markdown "Before.\n" "| a | b |\n|---|---|\n| 1 | 2 |\n" >>= errorsOf)
        `shouldReturn` [UnrepresentableContentError (fromList [TableContent])]

  describe "proseMirrorSteps" $ do
    it "fails with an unrepresentable content error for blocks ProseMirror can't represent" $
      (proseMirrorSteps ProseMirror Markdown proseMirrorHello "Hello\n\n| a | b |\n|---|---|\n| 1 | 2 |\n" >>= errorsOf)
        `shouldReturn` [UnrepresentableContentError (fromList [TableContent])]
