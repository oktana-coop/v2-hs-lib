module Error (Error (..), errorMessage) where

import qualified Data.Text as T
import Format (Format)

data Error
  = ReadError Format T.Text
  | WriteError Format T.Text
  | PatchError T.Text
  deriving (Show, Eq)

errorMessage :: Error -> T.Text
errorMessage (ReadError _ message) = message
errorMessage (WriteError _ message) = message
errorMessage (PatchError message) = message
