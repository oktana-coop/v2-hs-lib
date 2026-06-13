import Conversion.Golden as ConversionGolden (tests)
import Conversion.RoundTrip as ConversionRoundTrip (tests)
import Diff.Golden as DiffGolden (tests)
import Query.Spec as QuerySpec (tests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  conversionGoldenTests <- ConversionGolden.tests
  conversionRoundTripTests <- ConversionRoundTrip.tests
  diffGoldenTests <- DiffGolden.tests
  queryTests <- QuerySpec.tests
  defaultMain $ testGroup "Tests" [conversionGoldenTests, conversionRoundTripTests, diffGoldenTests, queryTests]
