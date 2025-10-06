module ProseMirror.Model where

import qualified ProseMirror.Json as PMJson (BlockNode (..), PMDoc (..))

class ToBlockNode a where
  toBlockNode :: a -> PMJson.BlockNode

class FromBlockNode a where
  fromBlockNode :: PMJson.BlockNode -> Either String a
