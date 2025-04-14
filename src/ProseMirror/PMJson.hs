module ProseMirror.PMJson (Doc, BlockNode (..), TextNode (..), Mark (..), Node (..)) where

import Data.Aeson (Object)
import qualified Data.Text as T

data Node = BlockNode BlockNode | TextNode TextNode deriving (Show, Eq)

data Mark = PMMark {markType :: T.Text, markAttrs :: Object} deriving (Show, Eq)

data TextNode = PMText {text :: T.Text, marks :: Maybe [Mark]} deriving (Show, Eq)

data BlockNode = PMBlock {nodeType :: T.Text, content :: Maybe [Node], attrs :: Maybe Object} deriving (Show, Eq)

type Doc = BlockNode