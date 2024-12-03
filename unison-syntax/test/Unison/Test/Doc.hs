module Unison.Test.Doc (test) where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import EasyTest
import Text.Megaparsec qualified as P
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Syntax.Lexer.Unison
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.Parser.Doc qualified as DP
import Unison.Syntax.Parser.Doc.Data qualified as Doc
import Unison.Util.Recursion

test :: Test ()
test =
  scope "Doc parser" . tests $
    [ t "# Hello" [Doc.Section (Doc.Paragraph $ docWord "Hello" :| []) []],
      t
        ( unlines
            [ "# Hello",
              "## Again"
            ]
        )
        [ Doc.Section
            (Doc.Paragraph $ docWord "Hello" :| [])
            [Fix $ Doc.Section (Doc.Paragraph $ docWord "Again" :| []) []]
        ],
      t
        ( unlines
            [ "## Hello",
              "# Again"
            ]
        )
        [ Doc.Section (Doc.Paragraph $ docWord "Hello" :| []) [],
          Doc.Section (Doc.Paragraph $ docWord "Again" :| []) []
        ],
      t
        "*some bold words*"
        [Doc.Paragraph' . Doc.Paragraph $ docBold (docWord "some" :| [docWord "bold", docWord "words"]) :| []],
      t
        "_some italic words_"
        [Doc.Paragraph' . Doc.Paragraph $ docItalic (docWord "some" :| [docWord "italic", docWord "words"]) :| []],
      t
        "~some struck-through words~"
        [ Doc.Paragraph' . Doc.Paragraph $
            docStrikethrough (docWord "some" :| [docWord "struck-through", docWord "words"]) :| []
        ],
      -- any number of emphasis delimiters is allowed
      t
        "__some italic words__"
        [Doc.Paragraph' . Doc.Paragraph $ docItalic (docWord "some" :| [docWord "italic", docWord "words"]) :| []],
      t
        "________some italic words________"
        [Doc.Paragraph' . Doc.Paragraph $ docItalic (docWord "some" :| [docWord "italic", docWord "words"]) :| []],
      t
        "***some bold words***"
        [ Doc.Paragraph' . Doc.Paragraph $ docBold (docWord "some" :| [docWord "bold", docWord "words"]) :| []
        ],
      t
        "***some _nested_ emphasis***"
        [ Doc.Paragraph' . Doc.Paragraph $
            docBold (docWord "some" :| [docItalic $ docWord "nested" :| [], docWord "emphasis"]) :| []
        ],
      -- mismatched delimiters should be preserved as text
      t "*" [Doc.Paragraph' . Doc.Paragraph $ docWord "*" :| []],
      t "`" [Doc.Paragraph' . Doc.Paragraph $ docWord "`" :| []],
      -- various code blocks (although we’re not testing the Unison code block lexer/parser with these)
      t
        ( unlines
            [ "```",
              "You might think this is code, but it’s not",
              "```"
            ]
        )
        [Doc.Eval "You might think this is code, but it’s not\n"],
      t
        ( unlines
            [ "`````````",
              "This one has extra delimiters",
              "`````````"
            ]
        )
        [Doc.Eval "This one has extra delimiters\n"],
      t
        ( unlines
            [ "``` unison",
              "You might think this is code, but it’s not",
              "```"
            ]
        )
        [Doc.CodeBlock "unison" "You might think this is code, but it’s not"],
      t
        ( unlines
            [ "````````` unison",
              "This one has extra delimiters",
              "`````````"
            ]
        )
        [Doc.CodeBlock "unison" "This one has extra delimiters"],
      t
        ( unlines
            [ "@typecheck ```",
              "You might think this is code, but it’s not",
              "```"
            ]
        )
        [Doc.ExampleBlock "\nYou might think this is code, but it’s not\n"],
      t
        ( unlines
            [ "@typecheck`````````",
              "This one has extra delimiters",
              "`````````"
            ]
        )
        [Doc.ExampleBlock "\nThis one has extra delimiters\n"],
      t "`some verbatim text`" [Doc.Paragraph' . Doc.Paragraph $ docCode "some verbatim text" :| []],
      t "''some verbatim text''" [Doc.Paragraph' . Doc.Paragraph $ docCode "some verbatim text" :| []],
      t "'''''some verbatim text'''''" [Doc.Paragraph' . Doc.Paragraph $ docCode "some verbatim text" :| []]
    ]

-- round-trip tests need to be in unison-parser-typechecker
--
-- -- want to get this to `Text` (or `String`), for round-trip testing
-- showPrettyDoc :: (Var v) => PrettyPrintEnv -> Term v a -> Pretty ColorText
-- showPrettyDoc ppe tm = PP.syntaxToColor . runPretty (avoidShadowing tm ppe) <$> prettyDoc2 emptyAc (printAnnotate ppe tm)

t ::
  String ->
  -- | Despite the long type, this is a simplified `Doc` – no annotations, and ident and code are Text & String,
  --   respectively.
  [Doc.Top String (Fix (Doc.Leaf Text String)) (Fix (Doc.Top String (Fix (Doc.Leaf Text String))))] ->
  Test ()
t s expected =
  scope s
    . either
      (crash . P.errorBundlePretty)
      ( \actual ->
          let expected' = Doc.UntitledSection $ embed <$> expected
              actual' = cata (\(_ :<< top) -> embed $ first (cata \(_ :<< leaf) -> embed leaf) top) <$> actual
           in if actual' == expected'
                then ok
                else do
                  note $ "expected: " ++ show expected'
                  note $ "actual  : " ++ show actual'
                  crash "actual != expected"
      )
    $ P.runParser (DP.doc (Name.toText . HQ'.toName . snd <$> typeOrTerm) (P.manyTill P.anySingle) P.eof) "test case" s

-- * Helper functions to make it easier to read the examples.

-- Once the parser gets generalized, these should be able to be removed, as they won’t require multiple layers of
-- constructor.

docBold :: NonEmpty (Fix (Doc.Leaf ident code)) -> Fix (Doc.Leaf ident code)
docBold = embed . Doc.Bold . Doc.Paragraph

docCode :: String -> Fix (Doc.Leaf ident code)
docCode = embed . Doc.Code . Doc.Word

docItalic :: NonEmpty (Fix (Doc.Leaf ident code)) -> Fix (Doc.Leaf ident code)
docItalic = embed . Doc.Italic . Doc.Paragraph

docStrikethrough :: NonEmpty (Fix (Doc.Leaf ident code)) -> Fix (Doc.Leaf ident code)
docStrikethrough = embed . Doc.Strikethrough . Doc.Paragraph

docWord :: String -> Fix (Doc.Leaf ident code)
docWord = embed . Doc.Word' . Doc.Word
