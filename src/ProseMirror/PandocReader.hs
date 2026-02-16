module ProseMirror.PandocReader (readProseMirror) where

import Control.Monad ((>=>))
import Control.Monad.Except (throwError)
import qualified Data.Text as T
import Data.Tree (Tree (..))
import qualified DocTree.GroupedInlines as GroupedInlinesTree
import ProseMirror.Model (PMDoc, parseProseMirrorText)
import ProseMirror.Tree (pmTreeFromPMDoc, pmTreeToGroupedInlinesTree)
import Text.Pandoc (PandocError (PandocParseError), ReaderOptions)
import Text.Pandoc.Builder as Pandoc (Pandoc)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Sources (ToSources, sourcesToText, toSources)

-- Although ReaderOptions are not used, the function is written like this so that it's consistent with the other Pandoc reader functions.
readProseMirror :: (PandocMonad m, ToSources a) => ReaderOptions -> a -> m Pandoc
readProseMirror _ =
  -- Using Kleisli composition to compose the 2 smaller functions in the monadic context (PandocMonad)
  parsePMJSONAndConvertToGroupedInlinesTree >=> GroupedInlinesTree.toPandoc
  where
    -- Parse ProseMirror JSON and convert to GroupedInlines tree.
    -- Lifts the Either-based computation into the PandocMonad, wrapping errors as PandocParseError
    parsePMJSONAndConvertToGroupedInlinesTree :: (ToSources a, PandocMonad m) => a -> m (Tree GroupedInlinesTree.DocNode)
    parsePMJSONAndConvertToGroupedInlinesTree = toPandocMonad . (parsePMJSON >=> pmDocToGroupedInlinesTree)
      where
        pmDocToGroupedInlinesTree :: PMDoc -> Either String (Tree GroupedInlinesTree.DocNode)
        pmDocToGroupedInlinesTree = pmTreeToGroupedInlinesTree . pmTreeFromPMDoc

        -- Parses ProseMirror JSON text to PMDoc
        parsePMJSON :: (ToSources a) => a -> Either String PMDoc
        parsePMJSON = parseProseMirrorText . sourcesToText . toSources

        toPandocMonad :: (PandocMonad m) => Either String a -> m a
        toPandocMonad = either (throwError . PandocParseError . T.pack) pure
