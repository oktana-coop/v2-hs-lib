module ProseMirror.Utils.Debug where

import Data.Tree (Tree (..), drawTree)
import qualified Debug.Trace

traceTree :: (Show a) => Tree a -> Tree a
traceTree tree = Debug.Trace.trace (drawTree $ fmap show tree) tree