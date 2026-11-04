module ProseMirror.Metadata (addDocFrontMatter, docFrontMatter) where

import qualified ProseMirror.Model as PM (Block (..), BlockNode (..), FrontMatter, Node (..), PMDoc (..))

-- The doc node with the front matter in its attribute. Other nodes are left as they are.
addDocFrontMatter :: Maybe PM.FrontMatter -> PM.Node -> PM.Node
addDocFrontMatter frontMatter (PM.BlockNode (PM.PMBlock (PM.Doc _) content)) = PM.BlockNode (PM.PMBlock (PM.Doc frontMatter) content)
addDocFrontMatter _ node = node

-- The front matter in the doc's attribute, if it has any.
docFrontMatter :: PM.PMDoc -> Maybe PM.FrontMatter
docFrontMatter (PM.PMDoc (PM.BlockNode (PM.PMBlock (PM.Doc frontMatter) _))) = frontMatter
docFrontMatter _ = Nothing
