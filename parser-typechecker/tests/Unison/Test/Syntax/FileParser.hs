module Unison.Test.Syntax.FileParser where

import Data.Functor.Identity (Identity (..))
import Data.List (uncons)
import Data.Set (elems)
import EasyTest
import Text.Megaparsec.Error qualified as MPE
import Unison.Parser.Ann qualified as P
import Unison.Parsers (unsafeGetRightFrom, unsafeParseFileBuiltinsOnly)
import Unison.PrintError (renderParseErrorAsANSI)
import Unison.Symbol (Symbol)
import Unison.Syntax.FileParser (file)
import Unison.Syntax.Parser qualified as P
import Unison.Test.Common qualified as Common
import Unison.UnisonFile (UnisonFile)
import Unison.Var (Var)

test1 :: Test ()
test1 =
  scope "test1" . tests . map parses $
    [ -- , "type () = ()\n()"
      "structural type Pair a b = Pair a b\n",
      "structural type Optional a = Just a | Nothing\n",
      unlines
        [ "structural type Optional2 a",
          "  = Just a",
          "  | Nothing\n"
        ],
      ------ -- ,unlines
      ------ --   ["structural type Optional a b c where"
      ------ --   ,"  Just : a -> Optional a"
      ------ --   ,"  Nothing : Optional Int"]
      ------ -- , unlines
      ------ --   ["structural type Optional"
      ------ --   ,"   a"
      ------ --   ,"   b"
      ------ --   ,"   c where"
      ------ --   ,"  Just : a -> Optional a"
      ------ --   ,"  Nothing : Optional Int"]
      unlines -- NB: this currently fails because we don't have type AST or parser for effect types yet
        [ "structural ability State s where",
          "  get : {State s} s",
          "  set : s -> {State s} ()"
        ],
      unlines
        [ "ping x = pong (x + 1)",
          "pong x = ping (x - 1)"
        ]
    ]

test2 :: Test ()
test2 =
  scope "test2" $
    (io $ unsafeParseFileBuiltinsOnly "unison-src/test1.u") *> ok

test :: Test ()
test =
  scope "fileparser" . tests $
    [ test1,
      emptyWatchTest,
      signatureNeedsAccompanyingBodyTest,
      emptyBlockTest,
      expectedBlockOpenTest
    ]

expectFileParseFailure :: String -> (P.Error Symbol -> Test ()) -> Test ()
expectFileParseFailure s expectation = scope s $ do
  let result = runIdentity (P.run (P.rootFile file) s Common.parsingEnv)
  case result of
    Right _ -> crash "Parser succeeded"
    Left (MPE.FancyError _ sets) ->
      case (fmap (fst) . uncons . elems) sets of
        Just (MPE.ErrorCustom e) -> expectation e
        Just _ -> crash "Error encountered was not custom"
        Nothing -> crash "No error found"
    Left e -> crash ("Parser failed with an error which was a trivial parser error: " ++ renderParseErrorAsANSI 80 s e)

emptyWatchTest :: Test ()
emptyWatchTest =
  scope "emptyWatchTest" $
    expectFileParseFailure ">" expectation
  where
    expectation :: (Var e) => P.Error e -> Test ()
    expectation e = case e of
      P.EmptyWatch _ann -> ok
      _ -> crash "Error wasn't EmptyWatch"

signatureNeedsAccompanyingBodyTest :: Test ()
signatureNeedsAccompanyingBodyTest =
  scope "signatureNeedsAccompanyingBodyTest" $
    expectFileParseFailure (unlines ["f : Nat -> Nat", "", "g a = a + 1"]) expectation
  where
    expectation :: (Var e) => P.Error e -> Test ()
    expectation e = case e of
      P.SignatureNeedsAccompanyingBody _ -> ok
      _ -> crash "Error wasn't SignatureNeedsAccompanyingBody"

emptyBlockTest :: Test ()
emptyBlockTest =
  scope "emptyBlockTest" $
    expectFileParseFailure (unlines ["f a =", "", "> 1 + 1"]) expectation
  where
    expectation :: (Var e) => P.Error e -> Test ()
    expectation e = case e of
      P.EmptyBlock _ -> ok
      _ -> crash "Error wasn't EmptyBlock"

expectedBlockOpenTest :: Test ()
expectedBlockOpenTest =
  scope "expectedBlockOpenTest" $
    expectFileParseFailure "f a b = match a b" expectation
  where
    expectation :: (Var e) => P.Error e -> Test ()
    expectation e = case e of
      P.ExpectedBlockOpen _ _ -> ok
      _ -> crash "Error wasn't ExpectedBlockOpen"

parses :: String -> Test ()
parses s = scope s $ do
  let p :: UnisonFile Symbol P.Ann
      !p =
        unsafeGetRightFrom s . runIdentity $
          P.run (P.rootFile file) s Common.parsingEnv
  pure p >> ok
