import Conversion.Golden as ConversionGolden (tests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  conversionGoldenTests <- ConversionGolden.tests
  defaultMain $ testGroup "Tests" [conversionGoldenTests]
