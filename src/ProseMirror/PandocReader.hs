module ProseMirror.PandocReader (readProseMirror) where

import Text.Pandoc (ReaderOptions)
import Text.Pandoc.Builder as Pandoc (Pandoc)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Sources (ToSources)

readProseMirror :: (PandocMonad m, ToSources a) => ReaderOptions -> a -> m Pandoc
readProseMirror = undefined