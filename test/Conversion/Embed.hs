module Conversion.Embed (tests) where

import Conversion (convertToText)
import qualified Data.Text as T
import Format (Format (Html, Markdown))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldContain, shouldNotContain)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

tests :: IO TestTree
tests = testSpec "Conversion.Embed" spec

-- Creates a temp directory containing a single (dummy) PNG asset, runs the
-- given action with that directory path, and cleans up afterwards.
withAssetDir :: (FilePath -> IO a) -> IO a
withAssetDir use =
  withSystemTempDirectory "v2-hs-lib-embed" $ \dir -> do
    -- The file contents are irrelevant: embedding only base64-encodes
    -- the bytes and infers the MIME type from the extension.
    writeFile (dir </> "img.png") "dummy-png-bytes"
    use dir

convertMdToHtml :: Maybe FilePath -> String -> IO String
convertMdToHtml resourcePath input = do
  result <- convertToText Markdown Html resourcePath input
  either (fail . show) (pure . T.unpack) result

spec :: Spec
spec =
  describe "Asset embedding in Markdown -> Html conversion" $ do
    it "embeds a referenced local asset as a data URI when a resource path is given" $
      withAssetDir $ \dir -> do
        html <- convertMdToHtml (Just dir) "![](img.png)"
        html `shouldContain` "data:image/png;base64,"

    it "leaves the relative src untouched when no resource path is given" $ do
      html <- convertMdToHtml Nothing "![](img.png)"
      html `shouldContain` "img.png"
      html `shouldNotContain` "data:image/png;base64,"
