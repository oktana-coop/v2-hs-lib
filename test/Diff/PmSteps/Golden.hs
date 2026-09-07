module Diff.PmSteps.Golden (tests) where

import Conversion (Format (..))
import Diff.Utils (readFilesAndProducePmSteps)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Utils (normalizeJson)

-- The steps of a case must turn its "before" document into its "after" document when applied with
-- prosemirror-transform; the golden files pin what hs-lib emits.
tests :: IO TestTree
tests = do
  return
    $ testGroup
      "Inputs → ProseMirror Steps (Golden)"
    $ [ testGroup
          "adding-and-removing-paragraphs"
          [ corpusCase $ "adding-and-removing-paragraphs" </> "add-paragraph-after-list",
            corpusCase $ "adding-and-removing-paragraphs" </> "add-paragraph-end",
            corpusCase $ "adding-and-removing-paragraphs" </> "add-paragraph-middle",
            corpusCase $ "adding-and-removing-paragraphs" </> "delete-last-paragraph",
            corpusCase $ "adding-and-removing-paragraphs" </> "delete-paragraph-middle"
          ],
        testGroup
          "horizontal-rule"
          [ corpusCase $ "horizontal-rule" </> "add-horizontal-rule",
            corpusCase $ "horizontal-rule" </> "delete-horizontal-rule"
          ],
        testGroup
          "images-and-figures"
          [ corpusCase $ "images-and-figures" </> "add-figure",
            corpusCase $ "images-and-figures" </> "add-captionless-figure",
            corpusCase $ "images-and-figures" </> "change-caption",
            corpusCase $ "images-and-figures" </> "change-image-src"
          ],
        testGroup
          "inline-text-updates"
          [ corpusCase $ "inline-text-updates" </> "add-text-in-the-middle-of-paragraph",
            corpusCase $ "inline-text-updates" </> "append-text-to-paragraph",
            corpusCase $ "inline-text-updates" </> "delete-text-from-the-end-of-paragraph",
            corpusCase $ "inline-text-updates" </> "delete-text-from-the-middle-of-paragraph",
            corpusCase $ "inline-text-updates" </> "edit-quoted-text",
            corpusCase $ "inline-text-updates" </> "change-quote-style"
          ],
        testGroup
          "lists"
          [ corpusCase $ "lists" </> "add-bullet-list",
            corpusCase $ "lists" </> "add-deeply-nested-list",
            corpusCase $ "lists" </> "add-item-in-the-middle-of-list",
            corpusCase $ "lists" </> "add-item-in-the-middle-of-list-and-rename-next-item",
            corpusCase $ "lists" </> "add-nested-blockquote-under-list-item",
            corpusCase $ "lists" </> "add-nested-list",
            corpusCase $ "lists" </> "add-nested-paragraph-under-list-item",
            corpusCase $ "lists" </> "add-second-nested-paragraph-under-list-item",
            corpusCase $ "lists" </> "append-item-to-list",
            corpusCase $ "lists" </> "delete-bullet-list",
            corpusCase $ "lists" </> "delete-last-item",
            corpusCase $ "lists" </> "delete-list-item-in-the-middle",
            corpusCase $ "lists" </> "delete-nested-lists",
            corpusCase $ "lists" </> "delete-paragraphs-nested-in-list-item"
          ],
        testGroup
          "marks"
          [ corpusCase $ "marks" </> "add-strong-emphasis-to-word",
            corpusCase $ "marks" </> "remove-strong-emphasis-from-word"
          ],
        testGroup
          "notes"
          [ corpusCase $ "notes" </> "add-note-in-the-end-of-document",
            corpusCase $ "notes" </> "add-note-in-the-end-of-paragraph",
            corpusCase $ "notes" </> "add-notes-just-before-and-after-another",
            corpusCase $ "notes" </> "delete-first-note",
            corpusCase $ "notes" </> "delete-note",
            corpusCase $ "notes" </> "delete-paragraphs-which-include-notes",
            corpusCase $ "notes" </> "edit-note-content",
            corpusCase $ "notes" </> "prepend-note"
          ],
        -- Rewrites are not collapsed for steps, so rewords stay small exact edits instead of a whole-span replace.
        testGroup
          "readability"
          [ corpusCase $ "readability" </> "light-reword",
            corpusCase $ "readability" </> "medium-reword",
            corpusCase $ "readability" </> "heavy-reword",
            corpusCase $ "readability" </> "reword-around-note"
          ],
        testGroup
          "unicode"
          [ corpusCase $ "unicode" </> "insert-text-after-emoji",
            corpusCase $ "unicode" </> "delete-text-after-emoji",
            corpusCase $ "unicode" </> "add-emphasis-after-emoji"
          ],
        testGroup
          "live-sync"
          [ goldenCase $ "live-sync" </> "multi-region",
            goldenCase $ "live-sync" </> "mark-toggle",
            goldenCase $ "live-sync" </> "heading-level",
            goldenCase $ "live-sync" </> "paragraph-split",
            goldenCase $ "live-sync" </> "paragraph-join",
            -- The "before" side is the editor's ProseMirror JSON (with the trailing paragraph stripped by the
            -- editor); the appended paragraph must land right after the list, i.e. at the end of the stripped doc.
            goldenCaseWithInputs ("live-sync" </> "trailing-paragraph") (ProseMirror, "doc1.json") (Markdown, "doc2.md")
          ]
      ]

stepsDir :: FilePath
stepsDir = "test/Diff/PmSteps"

-- Reuses the inputs of the decoration corpus (test/Diff/PmOutput/<case>); only the golden output lives here.
corpusCase :: FilePath -> TestTree
corpusCase caseSubFolderPath =
  let corpusDir = "test/Diff/PmOutput" </> caseSubFolderPath
   in goldenVsString
        caseSubFolderPath
        (stepsDir </> caseSubFolderPath </> "pm.json")
        (readFilesAndProducePmSteps Markdown Markdown normalizeJson (corpusDir </> "doc1.md") (corpusDir </> "doc2.md"))

goldenCase :: FilePath -> TestTree
goldenCase caseSubFolderPath = goldenCaseWithInputs caseSubFolderPath (Markdown, "doc1.md") (Markdown, "doc2.md")

goldenCaseWithInputs :: FilePath -> (Format, FilePath) -> (Format, FilePath) -> TestTree
goldenCaseWithInputs caseSubFolderPath (beforeFormat, beforeFile) (afterFormat, afterFile) =
  let baseDir = stepsDir </> caseSubFolderPath
   in goldenVsString
        caseSubFolderPath
        (baseDir </> "pm.json")
        (readFilesAndProducePmSteps beforeFormat afterFormat normalizeJson (baseDir </> beforeFile) (baseDir </> afterFile))
