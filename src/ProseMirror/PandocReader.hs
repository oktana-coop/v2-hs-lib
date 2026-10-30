module ProseMirror.PandocReader (readProseMirror) where

import Control.Monad ((>=>))
import Control.Monad.Except (throwError)
import qualified Data.Text as T
import Data.Tree (Tree (..))
import DocTree.Error (MalformedTreeError (..))
import qualified DocTree.GroupedInlines as GroupedInlinesTree
import ProseMirror.Metadata (metaFromPMDoc)
import ProseMirror.Model (PMDoc, parseProseMirrorText)
import ProseMirror.PandocTreeShape.FigureContent.GroupedInlines (wrapFigureContentInlinesToPlain)
import ProseMirror.PandocTreeShape.ImplicitFigure (addCaptionEqualToAlt, unwrapCaptionlessFigure)
import ProseMirror.Tree (pmTreeFromPMDoc, pmTreeToGroupedInlinesTree)
import Text.Pandoc (PandocError (PandocParseError), ReaderOptions)
import Text.Pandoc.Builder as Pandoc (Pandoc (..))
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Sources (ToSources, sourcesToText, toSources)

-- Although ReaderOptions are not used, the function is written like this so that it's consistent with the other Pandoc reader functions.
readProseMirror :: (PandocMonad m, ToSources a) => ReaderOptions -> a -> m Pandoc
readProseMirror _ =
  -- Using Kleisli composition to compose the functions in the monadic context (PandocMonad)
  toPandocMonad . parsePMJSON >=> pmDocToPandoc >=> pure . postprocessPandoc
  where
    -- The doc's metadata is read from its attribute, apart from the tree.
    pmDocToPandoc :: (PandocMonad m) => PMDoc -> m Pandoc
    pmDocToPandoc pmDoc = do
      meta <- toPandocMonad (metaFromPMDoc pmDoc)
      tree <- toPandocMonad (pmDocToGroupedInlinesTree pmDoc)
      Pandoc _ blocks <- treeToPandoc (wrapFigureContentInlinesToPlain tree)
      pure (Pandoc meta blocks)

    postprocessPandoc :: Pandoc -> Pandoc
    postprocessPandoc = unwrapCaptionlessFigure . addCaptionEqualToAlt

    treeToPandoc :: (PandocMonad m) => Tree GroupedInlinesTree.DocNode -> m Pandoc
    treeToPandoc = either (throwError . toPandocError) pure . GroupedInlinesTree.toPandoc

    toPandocError :: MalformedTreeError -> PandocError
    toPandocError (MalformedTreeError message) = PandocParseError message

    pmDocToGroupedInlinesTree :: PMDoc -> Either String (Tree GroupedInlinesTree.DocNode)
    pmDocToGroupedInlinesTree = pmTreeToGroupedInlinesTree . pmTreeFromPMDoc

    -- Parses ProseMirror JSON text to PMDoc
    parsePMJSON :: (ToSources a) => a -> Either String PMDoc
    parsePMJSON = parseProseMirrorText . sourcesToText . toSources

    toPandocMonad :: (PandocMonad m) => Either String a -> m a
    toPandocMonad = either (throwError . PandocParseError . T.pack) pure
