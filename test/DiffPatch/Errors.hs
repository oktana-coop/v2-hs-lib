{-# LANGUAGE OverloadedStrings #-}

module DiffPatch.Errors (tests) where

import Diffing (proseMirrorDiff)
import Error (Error (..))
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
  describe "proseMirrorDiff" $
    it "gives the read errors of both documents" $
      (proseMirrorDiff ProseMirror "{" "[" >>= errorsOf)
        `shouldReturn` [ ReadError ProseMirror "Unexpected end-of-input, expecting record key literal or }",
                         ReadError ProseMirror "Unexpected end-of-input, expecting JSON value or ]"
                       ]

  describe "proseMirrorSteps" $
    it "fails with a patch error when the steps can't be produced" $
      (proseMirrorSteps ProseMirror Markdown proseMirrorHello "Hello\n\n```{=html}\n<b>x</b>\n```\n" >>= errorsOf)
        `shouldReturn` [PatchError "Cannot emit steps: the document contains content with no ProseMirror representation."]
