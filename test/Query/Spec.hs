module Query.Spec (tests) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Format (Format (Markdown))
import Query (extractAssetUrls)
import System.FilePath ((<.>), (</>))
import Test.Hspec (Spec, describe, it, shouldReturn)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

tests :: IO TestTree
tests = testSpec "Query" spec

buildMdFilePathForCase :: FilePath -> FilePath
buildMdFilePathForCase caseName = "test/Query/AssetUrls" </> caseName <.> "md"

extractAssetUrlsForCase :: FilePath -> IO [T.Text]
extractAssetUrlsForCase caseName = do
  inputText <- TIO.readFile (buildMdFilePathForCase caseName)
  either (fail . show) return =<< extractAssetUrls Markdown (T.unpack inputText)

spec :: Spec
spec =
  describe "extractAssetUrls (Markdown)" $ do
    it "returns an empty list for content without images" $
      extractAssetUrlsForCase "no-images" `shouldReturn` []

    it "extracts inline image URLs in document order, handling titles and attributes" $
      extractAssetUrlsForCase "inline-images"
        `shouldReturn` map T.pack ["assets/x.png", "images/y.png", "z.png"]

    it "extracts the image URL from a figure" $
      extractAssetUrlsForCase "figure" `shouldReturn` [T.pack "assets/figure1.png"]

    it "returns relative paths as authored" $
      extractAssetUrlsForCase "relative-paths"
        `shouldReturn` map
          T.pack
          [ "./assets/image1.png",
            "../../assets/image1.png",
            "../sibling/image2.png"
          ]

    it "resolves reference-style images" $
      extractAssetUrlsForCase "reference-style" `shouldReturn` [T.pack "assets/foo.png"]

    it "ignores image syntax inside code fences, inline code and HTML comments" $
      extractAssetUrlsForCase "ignored-contexts" `shouldReturn` []

    it "returns external URLs as authored" $
      extractAssetUrlsForCase "external-urls"
        `shouldReturn` map
          T.pack
          [ "https://example.com/foo.png",
            "//example.com/bar.png",
            "data:image/png;base64,xyz"
          ]

    it "removes exact duplicate URLs, keeping first-occurrence order" $
      extractAssetUrlsForCase "duplicates"
        `shouldReturn` map T.pack ["assets/foo.png", "assets/bar.png"]
