module Query (extractAssetUrls) where

import Conversion (readDocument)
import Data.Bifunctor (bimap)
import Data.Containers.ListUtils (nubOrd)
import Data.List.NonEmpty (NonEmpty, singleton)
import qualified Data.Text as T
import Error (Error)
import Format (Format)
import Text.Pandoc (Pandoc)
import Text.Pandoc.Definition (Inline (Image))
import Text.Pandoc.Walk (query)

-- Returns the image URLs as authored in the document, in document order, with exact duplicates removed.
extractAssetUrls :: Format -> String -> IO (Either (NonEmpty Error) [T.Text])
extractAssetUrls format input = do
  eitherPandoc <- readDocument format (T.pack input)
  pure (bimap singleton imageUrls eitherPandoc)

imageUrls :: Pandoc -> [T.Text]
imageUrls = nubOrd . query imageUrl
  where
    imageUrl :: Inline -> [T.Text]
    imageUrl (Image _ _ (url, _)) = [url]
    imageUrl _ = []
