module ProseMirror.Error (PMConversionError (..)) where

data PMConversionError
  = -- Content that violates a node's spec in ProseMirror's schema (e.g. two images in a figure's content).
    NodeSpecViolationError String
  | -- A tree shape pandoc-tree doesn't build, so the conversion doesn't expect it (e.g. a figure child that is neither
    -- its content nor its caption).
    UnexpectedTreeShapeError String
  | -- A broken invariant of the conversion itself.
    InvariantError String
  deriving (Show, Eq)
