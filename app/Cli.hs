module Cli (Command (..), readInputCommand, Format (..)) where

import Conversion (Format (..))
import Options.Applicative (Parser, ReadM, argument, command, eitherReader, execParser, fullDesc, help, helper, info, long, metavar, option, progDesc, str, subparser, (<**>))

data Command
  = ConvertFromAutomerge Format String
  | ConvertToAutomerge Format String
  | ConvertToText Format Format String
  | ConvertToBinary Format Format String
  | ProseMirrorDiff Format String String
  | ExtractAssetUrls Format String
  deriving (Show)

formatParser :: String -> Parser Format
formatParser argName = option readFormat (long argName <> metavar "FORMAT" <> help "Specify the format (pandoc, markdown, html, json)")

readFormat :: ReadM Format
readFormat = eitherReader $ \arg ->
  case arg of
    "pandoc" -> Right Pandoc
    "markdown" -> Right Markdown
    "html" -> Right Html
    "json" -> Right Json
    "automerge" -> Right Automerge
    "prosemirror" -> Right ProseMirror
    "docx" -> Right Docx
    "pdf" -> Right Pdf
    _ -> Left $ "Unknown format: " ++ arg

commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "fromAutomerge"
        ( info
            (ConvertFromAutomerge <$> formatParser "to" <*> argument str (metavar "AUTOMERGE_SPANS_JSON"))
            (progDesc "Convert from Automerge Spans JSON to the output format")
        )
        <> command
          "toAutomerge"
          ( info
              (ConvertToAutomerge <$> formatParser "from" <*> argument str (metavar "INPUT_DATA"))
              (progDesc "Convert from the input format to Automerge Spans JSON")
          )
        <> command
          "convertToText"
          ( info
              (ConvertToText <$> formatParser "from" <*> formatParser "to" <*> argument str (metavar "INPUT_STRING"))
              (progDesc "Convert from the input to the output text format")
          )
        <> command
          "convertToBinary"
          ( info
              (ConvertToBinary <$> formatParser "from" <*> formatParser "to" <*> argument str (metavar "INPUT_STRING"))
              (progDesc "Convert from the input to the output binary format")
          )
        <> command
          "proseMirrorDiff"
          ( info
              (ProseMirrorDiff <$> formatParser "from" <*> argument str (metavar "DOC_1") <*> argument str (metavar "DOC_2"))
              (progDesc "Produce a ProseMirror document with the diff decorations given two input strings of the specified format")
          )
        <> command
          "extractAssetUrls"
          ( info
              (ExtractAssetUrls <$> formatParser "format" <*> argument str (metavar "INPUT_STRING"))
              (progDesc "Extract the URLs of asset references (e.g. images) from the input document")
          )
    )

readInputCommand :: IO Command
readInputCommand = execParser (info (commandParser <**> helper) fullDesc)
