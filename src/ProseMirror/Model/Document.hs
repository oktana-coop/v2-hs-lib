module ProseMirror.Model.Document (PMDoc (..), parseProseMirror, parseProseMirrorText, assertRootNodeIsDoc) where

import Control.Monad ((>=>))
import Data.Aeson (ToJSON (toJSON), eitherDecode, eitherDecodeStrictText)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import ProseMirror.Model.Node (Node, NodeType (DocType), nodeType)

data PMDoc = PMDoc {doc :: Node} deriving (Show, Eq)

instance ToJSON PMDoc where
  toJSON pmDoc = toJSON $ doc pmDoc

-- Using Kleisli composition to compose the 2 smaller functions in the monadic context (Either monad)
parseProseMirror :: BL.ByteString -> Either String PMDoc
parseProseMirror = eitherDecode >=> assertRootNodeIsDoc

-- Using Kleisli composition to compose the 2 smaller functions in the monadic context (Either monad)
parseProseMirrorText :: T.Text -> Either String PMDoc
parseProseMirrorText = eitherDecodeStrictText >=> assertRootNodeIsDoc

assertRootNodeIsDoc :: Node -> Either String PMDoc
assertRootNodeIsDoc n =
  if nodeType n == DocType
    then Right $ PMDoc {doc = n}
    else Left "Root node type is not doc"
