module ProseMirror.PandocReader (readProseMirror) where

import Control.Monad ((>=>))
import Control.Monad.Except (throwError)
import qualified Data.Text as T
import ProseMirror.PMJson (PMDoc, parseProseMirrorText)
import Text.Pandoc (PandocError (PandocParseError), ReaderOptions)
import Text.Pandoc.Builder as Pandoc (Pandoc)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Sources (ToSources, sourcesToText, toSources)

readProseMirror :: (PandocMonad m, ToSources a) => ReaderOptions -> a -> m Pandoc
readProseMirror _ =
  -- Using Kleisli composition to compose the 2 smaller functions in the monadic context (PandocMonad)
  parsePMJSONAndHandleErrors >=> toPandoc
  where
    -- Here we parse the JSON Automerge spans but also convert a potential error to a Pandoc parsing error
    -- The result is a list of Automerge spans wrapped within a Pandoc monad.
    parsePMJSONAndHandleErrors :: (ToSources a, PandocMonad m) => a -> m PMDoc
    parsePMJSONAndHandleErrors = either handleParsingErrorMessage pure . parsePMJSON

    parsePMJSON :: (ToSources a) => a -> Either String PMDoc
    parsePMJSON = parseProseMirrorText . sourcesToText . toSources

    handleParsingErrorMessage :: (PandocMonad m) => String -> m a
    handleParsingErrorMessage = throwError . PandocParseError . T.pack

toPandoc :: (PandocMonad m) => PMDoc -> m Pandoc.Pandoc
toPandoc = undefined