module Format (Format (..)) where

data Format = Pandoc | Markdown | Html | Json | Automerge | ProseMirror | Docx | Pdf deriving (Show)
