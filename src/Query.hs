module Query (extractAssetUrls) where

import Conversion (pandocReaderOptions, readFrom)
import Data.Bifunctor (bimap)
import Data.Containers.ListUtils (nubOrd)
import Data.List.NonEmpty (NonEmpty, singleton)
import qualified Data.Text as T
import Format (Format)
import Text.Pandoc (Pandoc, PandocError)
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Definition (Inline (Image))
import Text.Pandoc.Walk (query)

-- Returns the image URLs as authored in the document, in document order, with exact duplicates removed.
extractAssetUrls :: Format -> String -> IO (Either (NonEmpty PandocError) [T.Text])
extractAssetUrls format input = do
  eitherPandoc <- runIO (readFrom format pandocReaderOptions (T.pack input))
  pure (bimap singleton imageUrls eitherPandoc)

imageUrls :: Pandoc -> [T.Text]
imageUrls = nubOrd . query imageUrl
  where
    imageUrl :: Inline -> [T.Text]
    imageUrl (Image _ _ (url, _)) = [url]
    imageUrl _ = []
