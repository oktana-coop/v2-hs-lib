module Conversion.PmToPandoc.Golden (tests) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO as TIO
import ProseMirror.PandocReader (readProseMirror)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Text.Pandoc (def, runIO)

tests :: IO TestTree
tests = do
  return $ testGroup "ProseMirror → Pandoc (Golden)" [goldenCase "headings-and-paragraphs"]

goldenCase :: FilePath -> TestTree
goldenCase caseSubFolderPath =
  let baseDir = "test/Conversion/PmToPandoc" </> caseSubFolderPath
      pmInput = baseDir </> "pm.json"
      pandocGolden = baseDir </> "pandoc.json"
   in goldenVsString
        caseSubFolderPath
        pandocGolden
        (toPandocJson pmInput)

toPandocJson :: FilePath -> IO BL.ByteString
toPandocJson pmPath = do
  pmJson <- TIO.readFile pmPath
  result <- runIO $ readProseMirror def pmJson
  case result of
    Left err -> fail $ "Conversion to Pandoc failed: " ++ show err
    Right pandoc -> do
      -- Encode Pandoc as JSON for comparison
      return $ encode pandoc