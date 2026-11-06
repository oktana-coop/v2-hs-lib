{-# LANGUAGE OverloadedStrings #-}

module Markdown.FrontMatter.Invalid (invalidFrontMatter, pandocWithInvalidFrontMatter) where

import qualified Data.Map as M
import qualified Data.Text as T
import Text.Pandoc.Definition (Block, Meta (..), MetaValue (..), Pandoc (..))

-- Front matter Pandoc can't read travels inside the metadata as its raw text, under a temporary key of its own,
-- from a reader to a writer. Pandoc drops YAML keys that end with an underscore, so no front matter can have this key.
tempInvalidFrontMatterKey :: T.Text
tempInvalidFrontMatterKey = "tmp-invalid-front-matter_"

-- A document whose front matter Pandoc can't read: its raw text as the metadata, with the given body.
pandocWithInvalidFrontMatter :: T.Text -> [Block] -> Pandoc
pandocWithInvalidFrontMatter yaml blocks = Pandoc (Meta (M.singleton tempInvalidFrontMatterKey (MetaString yaml))) blocks

-- The raw text of front matter Pandoc couldn't read, if the metadata holds it.
invalidFrontMatter :: Meta -> Maybe T.Text
invalidFrontMatter (Meta metaMap) = case M.lookup tempInvalidFrontMatterKey metaMap of
  Just (MetaString yaml) -> Just yaml
  _ -> Nothing
