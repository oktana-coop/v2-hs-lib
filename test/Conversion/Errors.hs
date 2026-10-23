{-# LANGUAGE OverloadedStrings #-}

module Conversion.Errors (tests) where

import Conversion (convertToBinary, convertToText)
import qualified Data.Text as T
import Error (Error (..))
import Format (Format (Docx, Markdown, Pdf, ProseMirror))
import Test.Hspec (Spec, describe, it, shouldReturn, shouldSatisfy)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Utils (errorsOf)

tests :: IO TestTree
tests = testSpec "Conversion errors" spec

isYamlMetadataError :: Error -> Bool
isYamlMetadataError (InvalidMetadataError message) = "Error parsing YAML metadata" `T.isPrefixOf` message
isYamlMetadataError _ = False

spec :: Spec
spec = do
  describe "convertToBinary" $
    it "fails with a read error for a format it can't read" $
      (convertToBinary Pdf Docx Nothing "Hello" >>= errorsOf)
        `shouldReturn` [ReadError Pdf "Unknown input format 'pdf'\nPandoc can convert to PDF, but not from PDF."]

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

    it "fails with a metadata error, with Pandoc's message, for front matter that isn't valid YAML" $ do
      errors <- convertToText Markdown ProseMirror Nothing "---\ntitle: Notes\nauthor Ada: x: y\n---\n\nText.\n" >>= errorsOf
      errors `shouldSatisfy` all isYamlMetadataError

    it "fails with a metadata error for a YAML block further down the document" $ do
      errors <- convertToText Markdown ProseMirror Nothing "---\ntitle: Notes\n---\n\nText.\n\n---\nauthor Ada: x: y\n---\n\nMore text.\n" >>= errorsOf
      errors `shouldSatisfy` all isYamlMetadataError
