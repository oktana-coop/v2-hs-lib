module Error (Error (..), UnrepresentableContent (..)) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import Format (Format)
import ProseMirror.PandocTreeShape (UnrepresentableContent (..))

data Error
  = ReadError Format T.Text
  | InvalidMetadataError T.Text
  | WriteError Format T.Text
  | UnrepresentableContentError (NonEmpty UnrepresentableContent)
  | PatchError T.Text
  deriving (Show, Eq)
