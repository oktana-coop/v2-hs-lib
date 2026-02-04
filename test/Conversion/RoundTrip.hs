module Conversion.RoundTrip (tests) where

import Control.Monad (when)
import Conversion.Utils (toTextFormat)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Format (Format (Markdown, Pandoc, ProseMirror))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (<.>), (</>))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Utils (normalizeJson)

tests :: IO TestTree
tests = testSpec "Round-Trip Conversion" spec

spec :: Spec
spec = do
  describe "Markdown -> ProseMirror -> Markdown" $ do
    let inputFiles = map buildMdFilePathForCase ["blockquote", "headings-and-paragraphs", "marks", "lists", "notes"]
    -- roundTripTestForInputFile returns a monadic action (Spec is a monad)
    -- mapM_ combines these `Spec` actions into a single `Spec`.
    -- We use mapM_ because we don't care about the returned value, just the effect of these tests.
    mapM_ (roundTripTestForInputFile Markdown ProseMirror id) inputFiles
  describe "ProseMirror -> Markdown -> ProseMirror" $ do
    let inputFiles = map buildPmFilePathForCase ["blockquote", "headings-and-paragraphs", "marks", "lists", "notes"]
    -- roundTripTestForInputFile returns a monadic action (Spec is a monad)
    -- mapM_ combines these `Spec` actions into a single `Spec`.
    -- We use mapM_ because we don't care about the returned value, just the effect of these tests.
    mapM_ (roundTripTestForInputFile ProseMirror Markdown normalizeJson) inputFiles

buildMdFilePathForCase :: FilePath -> FilePath
buildMdFilePathForCase caseSubFolder = baseDir </> "doc.md"
  where
    baseDir = "test/Conversion/MdToPm" </> caseSubFolder

buildPmFilePathForCase :: FilePath -> FilePath
buildPmFilePathForCase caseSubFolder = baseDir </> "pm.json"
  where
    baseDir = "test/Conversion/PmToMd" </> caseSubFolder

roundTripTestForInputFile :: Format -> Format -> (T.Text -> T.Text) -> FilePath -> Spec
roundTripTestForInputFile ioFormat intermediaryFormat normalizer filePath = it filePath $ do
  inputText <- TIO.readFile filePath
  let normalizedInput = normalizer inputText
  intermediaryFormatText <- toTextFormat ioFormat intermediaryFormat inputText
  outputText <- toTextFormat intermediaryFormat ioFormat intermediaryFormatText
  let normalizedOutput = normalizer outputText

  -- Write files on failure to keep test-output clean
  when (normalizedInput /= normalizedOutput) (writeTestArtifactsToFiles normalizedInput intermediaryFormatText normalizedOutput)

  normalizedInput `shouldBe` normalizedOutput
  where
    writeTestArtifactsToFiles :: T.Text -> T.Text -> T.Text -> IO ()
    writeTestArtifactsToFiles input intermediary output = do
      let caseDir = takeDirectory filePath
      let logDir = "failed-round-trip-tests" </> caseDir

      createDirectoryIfMissing True logDir
      TIO.writeFile (logDir </> "input" <.> getFormatExtension ioFormat) input
      TIO.writeFile (logDir </> "intermediary" <.> getFormatExtension intermediaryFormat) intermediary
      TIO.writeFile (logDir </> "output" <.> getFormatExtension ioFormat) output

    getFormatExtension :: Format -> String
    getFormatExtension Markdown = "md"
    getFormatExtension ProseMirror = "json"
    getFormatExtension Pandoc = "txt"
    -- TODO: If needed, handle more formats.
    getFormatExtension _ = undefined
