module Diff.Golden (tests) where

import qualified Diff.ProseMirror.Golden as ProseMirrorDiffGolden
import Test.Tasty (TestTree, testGroup)

tests :: IO TestTree
tests = do
  proseMirrorDiffGoldenTests <- ProseMirrorDiffGolden.tests
  return $ testGroup "Diff Golden" [proseMirrorDiffGoldenTests]
