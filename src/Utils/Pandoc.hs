{-# LANGUAGE OverloadedStrings #-}

module Utils.Pandoc (isYamlMetadataError, withLog) where

import qualified Data.Text as T
import Text.Pandoc (PandocError (PandocParseError))
import Text.Pandoc.Class (PandocMonad, getLog)
import Text.Pandoc.Logging (LogMessage)

-- Pandoc says only in its error's text that a YAML metadata block isn't valid, wherever the block is in the
-- document.
isYamlMetadataError :: PandocError -> Bool
isYamlMetadataError (PandocParseError message) = "Error parsing YAML metadata" `T.isPrefixOf` message
isYamlMetadataError _ = False

withLog :: (PandocMonad m) => m a -> m (a, [LogMessage])
withLog action = liftA2 (,) action getLog
