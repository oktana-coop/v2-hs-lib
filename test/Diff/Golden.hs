module Diff.Golden (tests) where

import qualified Diff.PmOutput.Golden as PmOutputDiffGolden
import Test.Tasty (TestTree, testGroup)

tests :: IO TestTree
tests = do
  pmOutputDiffGoldenTests <- PmOutputDiffGolden.tests
  return $ testGroup "Diff Golden" [pmOutputDiffGoldenTests]