module Utils (normalizeJson, TextNormalizer) where

import qualified Data.Text as T
import Data.Aeson (Value, decode)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson.Encode.Pretty (Config, Indent (Spaces), confCompare, confIndent, defConfig, encodePretty')
import qualified Data.Text.Encoding as TE

type TextNormalizer = T.Text -> T.Text

-- `normalizeJson` uses `encodePretty'` to sort keys.
normalizeJson :: TextNormalizer
normalizeJson input = case decode (BL.fromStrict $ TE.encodeUtf8 input) :: Maybe Value of
  -- Return original text if invalid JSON
  Nothing -> input
  Just value -> TE.decodeUtf8 $ BL.toStrict (encodePretty' prettyConfig value)
  where
    prettyConfig :: Data.Aeson.Encode.Pretty.Config
    prettyConfig = defConfig {confCompare = compare, confIndent = Spaces 2}