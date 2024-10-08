{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.Referent where

import Data.Text qualified as Text
import EasyTest
import Unison.Reference qualified as Rf
import Unison.Referent qualified as R
import Unison.ShortHash qualified as SH

test :: Test ()
test =
  scope "hashparsing" . tests $
    [ scope "Reference" $
        tests
          [ ref h,
            ref (h <> "." <> suffix1),
            ref (h <> "." <> suffix2)
          ],
      scope "Referent" $
        tests
          [ r h,
            r $ h <> "." <> suffix1,
            r $ h <> "#d10",
            r $ h <> "#a0",
            r $ h <> "." <> suffix2 <> "#d6",
            r $ h <> "." <> suffix1 <> "#a9"
          ],
      scope "ShortHash" $
        tests
          [ sh h,
            sh "#abcd",
            sh $ "#abcd." <> suffix1,
            sh "#abcd#10",
            sh "#abcd#3",
            sh $ "#abcd." <> suffix2 <> "#10",
            sh $ "#abcd.6#5",
            scope "builtin" $
              expect (SH.fromText "##Text.take" == Just (SH.Builtin "Text.take")),
            pending $
              scope "builtins don't have CIDs" $
                expect (SH.fromText "##FileIO#3" == Nothing),
            scope "term ref, no cycle" $
              expect (SH.fromText "#2tWjVAuc7" == Just (SH.ShortHash "2tWjVAuc7" Nothing Nothing)),
            scope "term ref, part of cycle" $
              expect (SH.fromText "#y9ycWkiC1.1" == Just (SH.ShortHash "y9ycWkiC1" (Just 1) Nothing)),
            scope "constructor" $
              expect (SH.fromText "#cWkiC1x89#1" == Just (SH.ShortHash "cWkiC1x89" Nothing (Just 1))),
            scope "constructor of a type in a cycle" $
              expect (SH.fromText "#DCxrnCAPS.1#0" == Just (SH.ShortHash "DCxrnCAPS" (Just 1) (Just 0))),
            scope "Anything to the left of the first # is ignored" $
              expect (SH.fromText "foo#abc" == Just (SH.ShortHash "abc" Nothing Nothing)),
            pending $
              scope "Anything including and following a third # is rejected" $
                expect (SH.fromText "foo#abc#2#hello" == Nothing),
            scope "Anything after a second . before a second # is ignored" $
              expect (SH.fromText "foo#abc.1.x" == Just (SH.ShortHash "abc" (Just 1) Nothing))
          ]
    ]
  where
    h = "#1tdqrgl90qnmqvrff0j76kg2rnajq7n8j54e9cbk4p8pdi41q343bnh8h2rv6nadhlin8teg8371d445pvo0as7j2sav8k401d2s3no"
    suffix1 = show (0 :: Rf.Pos)
    suffix2 = show (3 :: Rf.Pos)
    ref txt = scope txt case Rf.fromText $ Text.pack txt of
      Left e -> fail e
      Right r1 -> case Rf.fromText (Rf.toText r1) of
        Left e -> fail e
        Right r2 -> expect (r1 == r2)
    r :: String -> Test ()
    r txt = scope txt case R.fromText $ Text.pack txt of
      Nothing -> fail "oh noes"
      Just referent -> case R.fromText (R.toText referent) of
        Nothing -> fail "oh noes"
        Just referent2 -> expect (referent == referent2)
    sh :: String -> Test ()
    sh txt = scope txt case SH.fromText $ Text.pack txt of
      Nothing -> fail "oh noes"
      Just shorthash -> case SH.fromText (SH.toText shorthash) of
        Nothing -> fail "oh noes"
        Just shorthash2 -> expect (shorthash == shorthash2)
