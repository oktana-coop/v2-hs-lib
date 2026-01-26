module Conversion.Golden (tests) where

import qualified Conversion.PmToPandoc.Golden as PmToPandocGolden
import Test.Tasty (TestTree, testGroup)

tests :: IO TestTree
tests = do
  pmToPandocGoldenTests <- PmToPandocGolden.tests
  return $ testGroup "Conversion Golden" [pmToPandocGoldenTests]