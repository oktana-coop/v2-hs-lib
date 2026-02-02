module Conversion.Golden (tests) where

import qualified Conversion.MdToPm.Golden as MdToPmGolden
import qualified Conversion.PmToMd.Golden as PmToMdGolden
import Test.Tasty (TestTree, testGroup)

tests :: IO TestTree
tests = do
  pmToMdGoldenTests <- PmToMdGolden.tests
  mdToPmGoldenTests <- MdToPmGolden.tests
  return $ testGroup "Conversion Golden" [pmToMdGoldenTests, mdToPmGoldenTests]