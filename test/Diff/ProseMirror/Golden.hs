module Diff.ProseMirror.Golden (tests) where

import Conversion (Format (Markdown))
import Diff.Utils (readFilesAndProducePmDiff, readFilesAndProducePmSteps)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Utils (normalizeJson)

tests :: IO TestTree
tests = do
  return
    $ testGroup
      "Markdown Inputs → ProseMirror Diff (Golden)"
    $ [ testGroup
          "adding-and-removing-paragraphs"
          [ goldenCase $ "adding-and-removing-paragraphs" </> "add-paragraph-after-list",
            goldenCase $ "adding-and-removing-paragraphs" </> "add-paragraph-end",
            goldenCase $ "adding-and-removing-paragraphs" </> "add-paragraph-middle",
            goldenCase $ "adding-and-removing-paragraphs" </> "delete-last-paragraph",
            goldenCase $ "adding-and-removing-paragraphs" </> "delete-paragraph-middle",
            goldenCase $ "adding-and-removing-paragraphs" </> "split-paragraph",
            goldenCase $ "adding-and-removing-paragraphs" </> "join-paragraphs"
          ],
        testGroup
          "headings"
          [ goldenCase $ "headings" </> "change-heading-level"
          ],
        testGroup
          "horizontal-rule"
          [ goldenCase $ "horizontal-rule" </> "add-horizontal-rule",
            goldenCase $ "horizontal-rule" </> "delete-horizontal-rule"
          ],
        testGroup
          "images-and-figures"
          [ goldenCase $ "images-and-figures" </> "add-figure",
            goldenCase $ "images-and-figures" </> "add-captionless-figure",
            goldenCase $ "images-and-figures" </> "change-caption",
            goldenCase $ "images-and-figures" </> "change-image-src"
          ],
        testGroup
          "inline-text-updates"
          [ goldenCase $ "inline-text-updates" </> "add-text-in-the-middle-of-paragraph",
            goldenCase $ "inline-text-updates" </> "append-text-to-paragraph",
            goldenCase $ "inline-text-updates" </> "delete-text-from-the-end-of-paragraph",
            goldenCase $ "inline-text-updates" </> "delete-text-from-the-middle-of-paragraph",
            goldenCase $ "inline-text-updates" </> "edit-quoted-text",
            -- Under smart reading both quote styles parse to the same `Quoted` node, so
            -- changing straight quotes to curly ones is not a visible edit.
            goldenCase $ "inline-text-updates" </> "change-quote-style",
            goldenCase $ "inline-text-updates" </> "edit-text-in-two-paragraphs"
          ],
        testGroup
          "lists"
          [ goldenCase $ "lists" </> "add-bullet-list",
            goldenCase $ "lists" </> "add-deeply-nested-list",
            goldenCase $ "lists" </> "add-item-in-the-middle-of-list",
            goldenCase $ "lists" </> "add-item-in-the-middle-of-list-and-rename-next-item",
            goldenCase $ "lists" </> "add-nested-blockquote-under-list-item",
            goldenCase $ "lists" </> "add-nested-list",
            goldenCase $ "lists" </> "add-nested-paragraph-under-list-item",
            goldenCase $ "lists" </> "add-second-nested-paragraph-under-list-item",
            goldenCase $ "lists" </> "append-item-to-list",
            goldenCase $ "lists" </> "delete-bullet-list",
            goldenCase $ "lists" </> "delete-last-item",
            goldenCase $ "lists" </> "delete-list-item-in-the-middle",
            goldenCase $ "lists" </> "delete-nested-lists",
            goldenCase $ "lists" </> "delete-paragraphs-nested-in-list-item"
          ],
        testGroup
          "marks"
          [ goldenCase $ "marks" </> "add-strong-emphasis-to-word",
            goldenCase $ "marks" </> "remove-strong-emphasis-from-word",
            goldenCase $ "marks" </> "replace-strong-emphasis-with-emphasis"
          ],
        testGroup
          "notes"
          [ goldenCase $ "notes" </> "add-note-in-the-end-of-document",
            goldenCase $ "notes" </> "add-note-in-the-end-of-paragraph",
            goldenCase $ "notes" </> "add-notes-just-before-and-after-another",
            goldenCase $ "notes" </> "delete-first-note",
            goldenCase $ "notes" </> "delete-note",
            goldenCase $ "notes" </> "delete-paragraphs-which-include-notes",
            goldenCase $ "notes" </> "edit-note-content",
            goldenCase $ "notes" </> "prepend-note"
          ],
        testGroup
          "readability"
          [ goldenCase $ "readability" </> "light-reword",
            goldenCase $ "readability" </> "medium-reword",
            goldenCase $ "readability" </> "heavy-reword",
            goldenCase $ "readability" </> "reword-around-note"
          ],
        -- ProseMirror positions count UTF-16 code units, so an emoji before an edit shifts it by two.
        testGroup
          "unicode"
          [ goldenCase $ "unicode" </> "insert-text-after-emoji",
            goldenCase $ "unicode" </> "delete-text-after-emoji",
            goldenCase $ "unicode" </> "add-emphasis-after-emoji"
          ]
      ]

casesDir :: FilePath
casesDir = "test/Diff/ProseMirror"

goldenCase :: FilePath -> TestTree
goldenCase caseSubFolderPath =
  testGroup
    caseSubFolderPath
    [ goldenVsString "decorated" (baseDir </> "decorated.json") (readFilesAndProducePmDiff Markdown normalizeJson doc1 doc2),
      goldenVsString "transformed" (baseDir </> "transformed.json") (readFilesAndProducePmSteps Markdown Markdown normalizeJson doc1 doc2)
    ]
  where
    baseDir = casesDir </> caseSubFolderPath
    doc1 = baseDir </> "doc1.md"
    doc2 = baseDir </> "doc2.md"
