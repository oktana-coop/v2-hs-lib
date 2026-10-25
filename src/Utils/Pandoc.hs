module Utils.Pandoc (withLog) where

import Text.Pandoc.Class (PandocMonad, getLog)
import Text.Pandoc.Logging (LogMessage)

withLog :: (PandocMonad m) => m a -> m (a, [LogMessage])
withLog action = liftA2 (,) action getLog
