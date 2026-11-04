{-# LANGUAGE OverloadedStrings #-}

module Conversion.FrontMatter (tests) where

import Conversion.Utils (toTextFormat)
import qualified Data.Text as T
import Format (Format (Json, ProseMirror))
import Test.Hspec (Spec, describe, it, shouldNotSatisfy)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

tests :: IO TestTree
tests = testSpec "ProseMirror Front Matter" spec

spec :: Spec
spec =
  describe "Pandoc JSON -> ProseMirror" $
    -- Pandoc's Markdown writer writes an empty block for such metadata.
    it "writes no front matter for metadata whose values are all empty" $ do
      output <- toTextFormat Json ProseMirror "{\"pandoc-api-version\":[1,23,1],\"meta\":{\"tags\":{\"t\":\"MetaList\",\"c\":[]}},\"blocks\":[{\"t\":\"Para\",\"c\":[{\"t\":\"Str\",\"c\":\"Text.\"}]}]}"
      output `shouldNotSatisfy` T.isInfixOf "frontMatter"
