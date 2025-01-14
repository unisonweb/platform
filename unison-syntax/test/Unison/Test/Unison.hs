module Unison.Test.Unison (test) where

import Data.Text qualified as Text
import EasyTest
import Unison.Prelude
import Unison.Syntax.HashQualifiedPrime qualified as HQ' (unsafeParseText)
import Unison.Syntax.Lexer.Unison

test :: Test ()
test =
  scope "lexer" . tests $
    [ t "" [],
      t "1" [Numeric "1"],
      t "+1" [Numeric "+1"],
      t "-1" [Numeric "-1"],
      t "-1.0" [Numeric "-1.0"],
      t "+1.0" [Numeric "+1.0"],
      t "1e3" [Numeric "1e3"],
      t "1e+3" [Numeric "1e+3"],
      t "1e-3" [Numeric "1e-3"],
      t "+1e3" [Numeric "+1e3"],
      t "+1e+3" [Numeric "+1e+3"],
      t "+1e-3" [Numeric "+1e-3"],
      t "-1e3" [Numeric "-1e3"],
      t "-1e+3" [Numeric "-1e+3"],
      t "-1e-3" [Numeric "-1e-3"],
      t "1.2e3" [Numeric "1.2e3"],
      t "1.2e+3" [Numeric "1.2e+3"],
      t "1.2e-3" [Numeric "1.2e-3"],
      t "+1.2e3" [Numeric "+1.2e3"],
      t "+1.2e+3" [Numeric "+1.2e+3"],
      t "+1.2e-3" [Numeric "+1.2e-3"],
      t "-1.2e3" [Numeric "-1.2e3"],
      t "-1.2e+3" [Numeric "-1.2e+3"],
      t "-1.2e-3" [Numeric "-1.2e-3"],
      t "1E3" [Numeric "1e3"],
      t "1E+3" [Numeric "1e+3"],
      t "1E-3" [Numeric "1e-3"],
      t "+1E3" [Numeric "+1e3"],
      t "+1E+3" [Numeric "+1e+3"],
      t "+1E-3" [Numeric "+1e-3"],
      t "-1E3" [Numeric "-1e3"],
      t "-1E+3" [Numeric "-1e+3"],
      t "-1E-3" [Numeric "-1e-3"],
      t "1.2E3" [Numeric "1.2e3"],
      t "1.2E+3" [Numeric "1.2e+3"],
      t "1.2E-3" [Numeric "1.2e-3"],
      t "+1.2E3" [Numeric "+1.2e3"],
      t "+1.2E+3" [Numeric "+1.2e+3"],
      t "+1.2E-3" [Numeric "+1.2e-3"],
      t "-1.2E3" [Numeric "-1.2e3"],
      t "-1.2E+3" [Numeric "-1.2e+3"],
      t "-1.2E-3" [Numeric "-1.2e-3"],
      t "1-1" [Numeric "1", simpleSymbolyId "-", Numeric "1"],
      t "1+1" [Numeric "1", simpleSymbolyId "+", Numeric "1"],
      t "1 +1" [Numeric "1", Numeric "+1"],
      t "1+ 1" [Numeric "1", simpleSymbolyId "+", Numeric "1"],
      t "x+y" [simpleWordyId "x", simpleSymbolyId "+", simpleWordyId "y"],
      t "++;++" [simpleSymbolyId "++", Semi False, simpleSymbolyId "++"],
      t "++; woot" [simpleSymbolyId "++", Semi False, simpleWordyId "woot"],
      t "woot;woot" [simpleWordyId "woot", Semi False, simpleWordyId "woot"],
      t "woot;(woot)" [simpleWordyId "woot", Semi False, Open "(", simpleWordyId "woot", Close],
      t
        "[+1,+1]"
        [Open "[", Numeric "+1", Reserved ",", Numeric "+1", Close],
      t
        "[ +1 , +1 ]"
        [Open "[", Numeric "+1", Reserved ",", Numeric "+1", Close],
      t "-- a comment 1.0" [],
      t "\"woot\" -- a comment 1.0" [Textual "woot"],
      t "0:Int" [Numeric "0", Reserved ":", simpleWordyId "Int"],
      t "0 : Int" [Numeric "0", Reserved ":", simpleWordyId "Int"],
      t
        ".Foo Foo `.` .foo.bar.baz"
        [ simpleWordyId ".Foo",
          simpleWordyId "Foo",
          simpleSymbolyId "`.`",
          simpleWordyId ".foo.bar.baz"
        ],
      t ".Foo.Bar.+" [simpleSymbolyId ".Foo.Bar.+"],
      t ".Foo.++.+" [simpleSymbolyId ".Foo.++.+"],
      t ".Foo.`++`.+" [simpleSymbolyId ".Foo.`++`.+"],
      t ".Foo.`+.+`.+" [simpleSymbolyId ".Foo.`+.+`.+"],
      -- idents with hashes
      t "foo#bar" [simpleWordyId "foo#bar"],
      t "+#bar" [simpleSymbolyId "+#bar"],
      -- note - these are all the same, just with different spacing
      let ex1 = "if x then y else z"
          ex2 = unlines ["if", "  x", "then", "  y", "else z"]
          ex3 = unlines ["if", "  x", "  then", "    y", "else z"]
          ex4 = unlines ["if", "  x", "  then", "  y", "else z"]
          expected =
            [ Open "if",
              simpleWordyId "x",
              Close,
              Open "then",
              simpleWordyId "y",
              Close,
              Open "else",
              simpleWordyId "z",
              Close
            ]
       in -- directly close empty = block
          tests $ map (`t` expected) [ex1, ex2, ex3, ex4],
      let ex = unlines ["test =", "", "x = 1"]
       in -- directly close nested empty blocks
          t
            ex
            [ simpleWordyId "test",
              Open "=",
              Close,
              (Semi True),
              simpleWordyId "x",
              Open "=",
              Numeric "1",
              Close
            ],
      let ex = unlines ["test =", "  test2 =", "", "x = 1"]
       in t
            ex
            [ simpleWordyId "test",
              Open "=",
              simpleWordyId "test2",
              Open "=",
              Close,
              Close,
              (Semi True),
              simpleWordyId "x",
              Open "=",
              Numeric "1",
              Close
            ],
      let ex =
            unlines
              ["if a then b", "else if c then d", "else if e then f", "else g"] -- close of the three `else` blocks
       in -- In an empty `then` clause, the `else` is interpreted as a `Reserved` token
          t
            ex
            [ Open "if",
              simpleWordyId "a",
              Close,
              Open "then",
              simpleWordyId "b",
              Close,
              Open "else",
              Open "if",
              simpleWordyId "c",
              Close,
              Open "then",
              simpleWordyId "d",
              Close,
              Open "else",
              Open "if",
              simpleWordyId "e",
              Close,
              Open "then",
              simpleWordyId "f",
              Close,
              Open "else",
              simpleWordyId "g",
              Close,
              Close,
              Close
            ],
      t
        "if x then else"
        [ Open "if",
          simpleWordyId "x",
          Close,
          Open "then",
          Close,
          Open "else",
          Close
        ],
      -- Empty `else` clause
      t
        "if x then 1 else"
        [ Open "if",
          simpleWordyId "x",
          Close,
          Open "then",
          Numeric "1",
          Close,
          Open "else",
          Close
        ],
      -- shouldn't be too eager to find keywords at the front of identifiers,
      -- particularly for block-closing keywords (see #2727)
      tests $ do
        kw <- ["if", "then", "else"]
        suffix <- ["0", "x", "!", "'"] -- examples of wordyIdChar
        let i = kw ++ suffix
        -- a keyword at the front of an identifier should still be an identifier
        pure $ t i [simpleWordyId (Text.pack i)],
      -- Test string literals
      t
        "\"simple string without escape characters\""
        [Textual "simple string without escape characters"],
      t
        "\"test escaped quotes \\\"in quotes\\\"\""
        [Textual "test escaped quotes \"in quotes\""],
      t "\"\\n \\t \\b \\a\"" [Textual "\n \t \b \a"],
      -- Delayed string
      t "'\"\"" [Reserved "'", Textual ""],
      -- https://github.com/unisonweb/unison/issues/4683
      -- don't emit virtual semis in ability lists or normal lists
      t "{foo\n,bar}" [Open "{", simpleWordyId "foo", Reserved ",", simpleWordyId "bar", Close],
      t "{foo\n ,bar}" [Open "{", simpleWordyId "foo", Reserved ",", simpleWordyId "bar", Close],
      t "[foo\n,bar]" [Open "[", simpleWordyId "foo", Reserved ",", simpleWordyId "bar", Close],
      t "[foo\n ,bar]" [Open "[", simpleWordyId "foo", Reserved ",", simpleWordyId "bar", Close]
    ]

t :: String -> [Lexeme] -> Test ()
t s expected = case toList . preParse $ lexer filename s of
  [token@(Token (Err _) _ _)] -> crash $ show token
  tokened ->
    let actual = payload <$> tokened
        expected' = Open filename : expected <> pure Close
     in scope s $
          if actual == expected'
            then ok
            else do
              note $ "expected: " ++ show expected'
              note $ "actual  : " ++ show actual
              crash "actual != expected"
  where
    filename = "test case"

simpleSymbolyId :: Text -> Lexeme
simpleSymbolyId =
  SymbolyId . HQ'.unsafeParseText

simpleWordyId :: Text -> Lexeme
simpleWordyId =
  WordyId . HQ'.unsafeParseText
