{-# LANGUAGE OverloadedStrings #-}

module Conversion.FrontMatter (tests) where

import Conversion.Utils (toTextFormat)
import Data.List.NonEmpty (fromList)
import qualified Data.Text as T
import Error (Error (..), UnrepresentableContent (..))
import Format (Format (Json, ProseMirror))
import Normalization (normalizeFrontMatterForProseMirror)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotSatisfy, shouldSatisfy)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

tests :: IO TestTree
tests = testSpec "ProseMirror Front Matter" spec

isYamlMetadataError :: Error -> Bool
isYamlMetadataError (InvalidMetadataError message) = "Error parsing YAML metadata" `T.isPrefixOf` message
isYamlMetadataError _ = False

spec :: Spec
spec = do
  describe "Pandoc JSON -> ProseMirror" $
    -- Pandoc's Markdown writer writes an empty block for such metadata.
    it "writes no front matter for metadata whose values are all empty" $ do
      output <- toTextFormat Json ProseMirror "{\"pandoc-api-version\":[1,23,1],\"meta\":{\"tags\":{\"t\":\"MetaList\",\"c\":[]}},\"blocks\":[{\"t\":\"Para\",\"c\":[{\"t\":\"Str\",\"c\":\"Text.\"}]}]}"
      output `shouldNotSatisfy` T.isInfixOf "frontMatter"

  describe "normalizeFrontMatterForProseMirror" $ do
    it "keeps the order of the keys" $
      normalizeFrontMatterForProseMirror "zeta: 1\nalpha: 2\n" `shouldBe` Right (Just "zeta: 1\nalpha: 2\n")

    it "drops an HTML comment in a value" $
      normalizeFrontMatterForProseMirror "title: Hello <!-- a comment --> world\n" `shouldBe` Right (Just "title: Hello world\n")

    it "gives no front matter for values that are all empty" $
      normalizeFrontMatterForProseMirror "title: \"\"\ntags: []\n" `shouldBe` Right Nothing

    it "fails with a metadata error, with Pandoc's message, for YAML that isn't valid" $
      normalizeFrontMatterForProseMirror "title: Notes\nauthor Ada: x: y\n" `shouldSatisfy` either (all isYamlMetadataError) (const False)

    it "fails with a metadata error for a line that would end the front matter" $
      normalizeFrontMatterForProseMirror "a: 1\n---\nb: 2\n" `shouldBe` Left (pure (InvalidMetadataError "A line with only --- or ... would end the front matter."))

    it "fails with an unrepresentable content error for a note in a value" $
      normalizeFrontMatterForProseMirror "title: A title^[A note.]\n" `shouldBe` Left (pure (UnrepresentableContentError (fromList [MetadataNoteContent])))
