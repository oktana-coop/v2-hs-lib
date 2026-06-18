module Conversion.Unsupported (tests) where

import Conversion (convertToText)
import Data.Either (isLeft)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Format (Format (Markdown, ProseMirror))
import System.FilePath ((<.>), (</>))
import Test.Hspec (Spec, describe, it, shouldReturn)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

tests :: IO TestTree
tests = testSpec "Unsupported input (conversion errors)" spec

buildMdFilePathForCase :: FilePath -> FilePath
buildMdFilePathForCase caseName = "test/Conversion/Unsupported" </> caseName <.> "md"

-- Content with no ProseMirror representation must fail loudly rather than be silently dropped.
conversionFailsForCase :: FilePath -> IO Bool
conversionFailsForCase caseName = do
  inputText <- TIO.readFile (buildMdFilePathForCase caseName)
  isLeft <$> convertToText Markdown ProseMirror Nothing (T.unpack inputText)

spec :: Spec
spec =
  describe "Markdown → ProseMirror rejects unrepresentable content" $ do
    it "errors on a figure whose caption can only be represented as raw HTML" $
      conversionFailsForCase "figure-with-caption" `shouldReturn` True

    it "errors on a multi-image figure (raw HTML)" $
      conversionFailsForCase "multi-image-figure" `shouldReturn` True

    it "errors on an arbitrary raw HTML block" $
      conversionFailsForCase "raw-html-block" `shouldReturn` True
