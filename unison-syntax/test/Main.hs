module Main (main) where

import EasyTest
import System.Environment (getArgs)
import System.IO
import System.IO.CodePage (withCP65001)
import Unison.Test.Doc qualified as Doc
import Unison.Test.Unison qualified as Unison

-- main :: IO ()
-- main = withCP65001 . run $ tests [Unison.test, Doc.test]

test :: Test ()
test =
  tests
    [ Doc.test,
      Unison.test
    ]

main :: IO ()
main = withCP65001 do
  args <- getArgs
  mapM_ (`hSetEncoding` utf8) [stdout, stdin, stderr]
  case args of
    [] -> runOnly "" test
    [prefix] -> runOnly prefix test
    [seed, prefix] -> rerunOnly (read seed) prefix test
    _ -> error "expected no args, a prefix, or a seed and a prefix"
