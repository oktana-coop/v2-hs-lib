{-# LANGUAGE OverloadedStrings #-}

module Conversion.Errors (tests) where

import Conversion (convertToBinary, convertToText, toWriteError)
import Data.Either (isRight)
import Data.List.NonEmpty (fromList)
import qualified Data.Text as T
import Error (Error (..), UnrepresentableContent (..))
import Format (Format (Docx, Html, Markdown, Pdf, ProseMirror))
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Pandoc (PandocError (PandocShouldNeverHappenError))
import Utils (errorsOf)

tests :: IO TestTree
tests = testSpec "Conversion errors" spec

isYamlMetadataError :: Error -> Bool
isYamlMetadataError (InvalidMetadataError message) = "Error parsing YAML metadata" `T.isPrefixOf` message
isYamlMetadataError _ = False

asksToReportToPandoc :: Error -> Bool
asksToReportToPandoc (WriteError _ message) = "Please report this to pandoc's developers" `T.isInfixOf` message
asksToReportToPandoc _ = False

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

    it "fails with a read error for ProseMirror JSON whose tree pandoc-tree can't map" $
      (convertToText ProseMirror Markdown Nothing "{\"type\":\"doc\",\"content\":[{\"type\":\"figure\",\"content\":[{\"type\":\"figure_content\",\"content\":[{\"type\":\"image\",\"attrs\":{\"src\":\"a.png\"}}]},{\"type\":\"caption\",\"content\":[{\"type\":\"text\",\"text\":\"One\"}]},{\"type\":\"caption\",\"content\":[{\"type\":\"text\",\"text\":\"Two\"}]}]}]}" >>= errorsOf)
        `shouldReturn` [ReadError ProseMirror "Found orphan inline node"]

    it "fails with an unrepresentable content error, naming each kind once in document order, for blocks ProseMirror can't hold" $
      (convertToText Markdown ProseMirror Nothing "Term\n: Definition\n\n| a | b |\n|---|---|\n| 1 | 2 |\n\n| a line\n\n```{=html}\n<b>x</b>\n```\n\nTerm\n: Definition\n" >>= errorsOf)
        `shouldReturn` [UnrepresentableContentError (fromList [DefinitionListContent, TableContent, LineBlockContent, RawBlockContent])]

    it "finds the blocks ProseMirror can't hold inside other blocks" $
      (convertToText Markdown ProseMirror Nothing "> | a | b |\n> |---|---|\n> | 1 | 2 |\n" >>= errorsOf)
        `shouldReturn` [UnrepresentableContentError (fromList [TableContent])]

    it "doesn't fail an export for blocks ProseMirror can't represent" $ do
      result <- convertToText Markdown Html Nothing "| a | b |\n|---|---|\n| 1 | 2 |\n"
      result `shouldSatisfy` isRight

    it "fails with a write error, with the writer's message, for a figure ProseMirror can't hold" $
      (convertToText Html ProseMirror Nothing "<figure><img src=\"a.png\"><img src=\"b.png\"></figure>" >>= errorsOf)
        `shouldReturn` [WriteError ProseMirror "The figure content must contain at most one image (no other inline content)"]

    it "fails with a metadata error, with Pandoc's message, for front matter that isn't valid YAML" $ do
      errors <- convertToText Markdown ProseMirror Nothing "---\ntitle: Notes\nauthor Ada: x: y\n---\n\nText.\n" >>= errorsOf
      errors `shouldSatisfy` all isYamlMetadataError

    it "fails with a metadata error for a YAML block further down the document" $ do
      errors <- convertToText Markdown ProseMirror Nothing "---\ntitle: Notes\n---\n\nText.\n\n---\nauthor Ada: x: y\n---\n\nMore text.\n" >>= errorsOf
      errors `shouldSatisfy` all isYamlMetadataError

  describe "toWriteError" $ do
    it "keeps only the message of the ProseMirror writer's broken invariants" $
      toWriteError ProseMirror (PandocShouldNeverHappenError "No root node found.")
        `shouldBe` WriteError ProseMirror "No root node found."

    it "keeps Pandoc's whole message for its own writers' broken invariants, which are Pandoc's bugs" $
      toWriteError Html (PandocShouldNeverHappenError "Broken.") `shouldSatisfy` asksToReportToPandoc
