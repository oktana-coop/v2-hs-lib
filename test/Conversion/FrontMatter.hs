{-# LANGUAGE OverloadedStrings #-}

module Conversion.FrontMatter (tests) where

import Control.Monad (forM_)
import Conversion.Normalization.Golden (NormalizationCase (..), inputPath, metaNormalizationCases, normalizedPath)
import Conversion.Utils (toTextFormat)
import Data.Aeson (Value (..), decodeStrictText)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List.NonEmpty (fromList)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Error (Error (..), UnrepresentableContent (..))
import Format (Format (Json, Markdown, ProseMirror))
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

  describe "Normalization cases -> ProseMirror" $
    forM_ metaNormalizationCases $ \normalizationCase ->
      it ("holds the front matter of the normalized document: " <> caseSubFolderPath normalizationCase) $ do
        frontMatter <- convertedPMDocFrontMatter normalizationCase
        expected <- normalizedDocumentFrontMatter normalizationCase
        frontMatter `shouldBe` expected

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

    -- Only a Markdown input has front matter.
    forM_ (filter ((== Markdown) . inputFormat) metaNormalizationCases) $ \normalizationCase ->
      it ("gives the front matter of the normalized document from the input's: " <> caseSubFolderPath normalizationCase) $ do
        input <- TIO.readFile (inputPath normalizationCase)
        expected <- normalizedDocumentFrontMatter normalizationCase
        fmap normalizeFrontMatterForProseMirror (frontMatterOf input) `shouldBe` Just (Right expected)

-- The front matter in the doc's attribute of the case's input converted to ProseMirror.
convertedPMDocFrontMatter :: NormalizationCase -> IO (Maybe T.Text)
convertedPMDocFrontMatter normalizationCase = do
  input <- TIO.readFile (inputPath normalizationCase)
  pmJson <- toTextFormat (inputFormat normalizationCase) ProseMirror input
  pure (docFrontMatter pmJson)
  where
    docFrontMatter :: T.Text -> Maybe T.Text
    docFrontMatter pmJson = do
      Object doc <- decodeStrictText pmJson
      Object attrs <- KeyMap.lookup "attrs" doc
      String frontMatter <- KeyMap.lookup "frontMatter" attrs
      pure frontMatter

normalizedDocumentFrontMatter :: NormalizationCase -> IO (Maybe T.Text)
normalizedDocumentFrontMatter normalizationCase = frontMatterOf <$> TIO.readFile (normalizedPath normalizationCase)

-- The YAML between the delimiters of a Markdown document's front matter.
frontMatterOf :: T.Text -> Maybe T.Text
frontMatterOf markdown = case T.lines markdown of
  "---" : rest -> Just (T.unlines (takeWhile (/= "---") rest))
  _ -> Nothing
