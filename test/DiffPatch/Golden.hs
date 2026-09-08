module DiffPatch.Golden (tests) where

import qualified DiffPatch.ProseMirror.Golden as ProseMirrorDiffPatchGolden
import Test.Tasty (TestTree, testGroup)

tests :: IO TestTree
tests = do
  proseMirrorDiffPatchGoldenTests <- ProseMirrorDiffPatchGolden.tests
  return $ testGroup "Diff & Patch Golden" [proseMirrorDiffPatchGoldenTests]
