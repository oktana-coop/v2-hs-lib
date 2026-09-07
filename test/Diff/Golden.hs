module Diff.Golden (tests) where

import qualified Diff.PmOutput.Golden as PmOutputDiffGolden
import qualified Diff.PmSteps.Golden as PmStepsGolden
import Test.Tasty (TestTree, testGroup)

tests :: IO TestTree
tests = do
  pmOutputDiffGoldenTests <- PmOutputDiffGolden.tests
  pmStepsGoldenTests <- PmStepsGolden.tests
  return $ testGroup "Diff Golden" [pmOutputDiffGoldenTests, pmStepsGoldenTests]
