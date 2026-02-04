module Diff.Utils (readFilesAndProducePmDiff) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Data.List.NonEmpty (head)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Diff (proseMirrorDiff)
import Format (Format)

readFilesAndProducePmDiff :: Format -> FilePath -> FilePath -> IO BL.ByteString
readFilesAndProducePmDiff inputFormat input1FilePath input2FilePath = do
  input1Text <- TIO.readFile input1FilePath
  input2Text <- TIO.readFile input2FilePath
  eitherOutputText <- proseMirrorDiff inputFormat (T.unpack input1Text) (T.unpack input2Text)
  case eitherOutputText of
    Left errors -> fail ("Conversion failed: " <> (show $ Data.List.NonEmpty.head errors))
    Right outputText -> (return . encode) outputText