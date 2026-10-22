import Conversion.Embed as ConversionEmbed (tests)
import Conversion.Golden as ConversionGolden (tests)
import Conversion.MetaKeyOrder as ConversionMetaKeyOrder (tests)
import Conversion.ReaderAgreement as ConversionReaderAgreement (tests)
import Conversion.RoundTrip as ConversionRoundTrip (tests)
import Conversion.Unsupported as ConversionUnsupported (tests)
import DiffPatch.Golden as DiffPatchGolden (tests)
import DiffPatch.MetaKeyOrder as DiffPatchMetaKeyOrder (tests)
import Query.Spec as QuerySpec (tests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  conversionGoldenTests <- ConversionGolden.tests
  conversionRoundTripTests <- ConversionRoundTrip.tests
  conversionMetaKeyOrderTests <- ConversionMetaKeyOrder.tests
  conversionUnsupportedTests <- ConversionUnsupported.tests
  conversionEmbedTests <- ConversionEmbed.tests
  conversionReaderAgreementTests <- ConversionReaderAgreement.tests
  diffPatchGoldenTests <- DiffPatchGolden.tests
  diffPatchMetaKeyOrderTests <- DiffPatchMetaKeyOrder.tests
  queryTests <- QuerySpec.tests
  defaultMain $ testGroup "Tests" [conversionGoldenTests, conversionRoundTripTests, conversionMetaKeyOrderTests, conversionUnsupportedTests, conversionEmbedTests, conversionReaderAgreementTests, diffPatchGoldenTests, diffPatchMetaKeyOrderTests, queryTests]
