import Conversion.Golden as ConversionGolden (tests)
import Conversion.RoundTrip as ConversionRoundTrip (tests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  conversionGoldenTests <- ConversionGolden.tests
  conversionRoundTripTests <- ConversionRoundTrip.tests
  defaultMain $ testGroup "Tests" [conversionGoldenTests, conversionRoundTripTests]
