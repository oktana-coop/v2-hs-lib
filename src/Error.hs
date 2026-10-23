module Error (Error (..)) where

import qualified Data.Text as T
import Format (Format)

data Error
  = ReadError Format T.Text
  | InvalidMetadataError T.Text
  | WriteError Format T.Text
  | PatchError T.Text
  deriving (Show, Eq)
