{-# LANGUAGE OverloadedStrings #-}

module Conversion.Errors (tests) where

import Conversion (convertToText)
import Error (Error (..))
import Format (Format (Markdown, Pdf, ProseMirror))
import Test.Hspec (Spec, describe, it, shouldReturn)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Utils (errorsOf)

tests :: IO TestTree
tests = testSpec "Conversion errors" spec

spec :: Spec
spec =
  describe "convertToText" $ do
    it "fails with a read error for a format it can't read, without crashing" $
      (convertToText Pdf Markdown Nothing "Hello" >>= errorsOf)
        `shouldReturn` [ReadError Pdf "Unknown input format 'pdf'\nPandoc can convert to PDF, but not from PDF."]

    it "fails with a read error for ProseMirror JSON that doesn't fit the schema" $
      (convertToText ProseMirror Markdown Nothing "{\"type\":\"nope\"}" >>= errorsOf)
        `shouldReturn` [ReadError ProseMirror "Error in $.type: Invalid block type"]

    it "fails with a write error for content ProseMirror can't hold" $
      (convertToText Markdown ProseMirror Nothing "```{=html}\n<b>x</b>\n```\n" >>= errorsOf)
        `shouldReturn` [WriteError ProseMirror "Cannot convert input to ProseMirror: it contains content with no ProseMirror representation."]
