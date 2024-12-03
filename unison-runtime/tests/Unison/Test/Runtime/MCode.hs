{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeApplications #-}

module Unison.Test.Runtime.MCode where

import Control.Concurrent.STM
import Data.Map.Strict qualified as Map
import EasyTest
import Unison.Reference (Reference, Reference' (Builtin))
import Unison.Runtime.ANF
  ( Cacheability (..),
    Code (..),
    SuperGroup (..),
    lamLift,
    superNormalize,
  )
import Unison.Runtime.Machine
  ( CCache (..),
    apply0,
    baseCCache,
    cacheAdd,
  )
import Unison.Runtime.Pattern
import Unison.Symbol (Symbol)
import Unison.Term (unannotate)
import Unison.Test.Common (tm)

dummyRef :: Reference
dummyRef = Builtin "dummy"

mainRef :: Reference
mainRef = Builtin "main"

modifyTVarTest :: TVar a -> (a -> a) -> Test ()
modifyTVarTest v f = io . atomically $ modifyTVar v f

testEval0 :: [(Reference, SuperGroup Symbol)] -> SuperGroup Symbol -> Test ()
testEval0 env main =
  ok << io do
    cc <- baseCCache False
    _ <- cacheAdd ((fmap . fmap) uncacheable $ (mainRef, main) : env) cc
    rtm <- readTVarIO (refTm cc)
    apply0 Nothing cc Nothing (rtm Map.! mainRef)
  where
    (<<) = flip (>>)
    uncacheable sg = CodeRep sg Uncacheable

multRec :: String
multRec =
  "let\n\
  \  n = 5\n\
  \  f acc i = match i with\n\
  \    0 -> acc\n\
  \    _ -> f (##Nat.+ acc n) (##Nat.sub i 1)\n\
  \  if (##Nat.== (f 0 1000) 5000) then () else ##bug ()"

testEval :: String -> Test ()
testEval s = testEval0 (fmap superNormalize <$> ctx) (superNormalize ll)
  where
    (ll, _, ctx, _) =
      lamLift mempty
        . splitPatterns builtinDataSpec
        . unannotate
        $ tm s

nested :: String
nested =
  "let\n\
  \  x = match 2 with\n\
  \        0 -> ##Nat.+ 0 1\n\
  \        m@n -> n\n\
  \  if (##Nat.== x 2) then () else ##bug ()"

matching'arguments :: String
matching'arguments =
  "let\n\
  \  f x y z = y\n\
  \  g x = f x\n\
  \  blorf = let\n\
  \    a = 0\n\
  \    b = 1\n\
  \    d = 2\n\
  \    h = g a b\n\
  \    c = 2\n\
  \    h c\n\
  \  if (##Nat.== blorf 1) then () else ##bug ()"

test :: Test ()
test =
  scope "mcode" . tests $
    [ scope "2=2" $ testEval "if (##Nat.== 2 2) then () else ##bug ()",
      scope "2=1+1" $ testEval "if (##Nat.== 2 (##Nat.+ 1 1)) then () else ##bug ()",
      scope "2=3-1" $ testEval "if (##Nat.== 2 (##Nat.sub 3 1)) then () else ##bug ()",
      scope "5*5=25" $
        testEval "if (##Nat.== (##Nat.* 5 5) 25) then () else ##bug ()",
      scope "5*1000=5000" $
        testEval "if (##Nat.== (##Nat.* 5 1000) 5000) then () else ##bug ()",
      scope "5*1000=5000 rec" $ testEval multRec,
      scope "nested" $
        testEval nested,
      scope "matching arguments" $
        testEval matching'arguments
    ]
