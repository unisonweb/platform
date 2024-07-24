{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.Term where

import Data.Map ((!))
import Data.Map qualified as Map
import Data.Text.Encoding (encodeUtf8)
import EasyTest
import Unison.Hash qualified as Hash
import Unison.Reference qualified as R
import Unison.Symbol (Symbol)
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Var qualified as Var

test :: Test ()
test =
  scope "term" $
    tests
      [ scope "Term.substTypeVar" $ do
          -- check that capture avoidance works in substTypeVar
          let v s = Var.nameds s :: Symbol
              tv s = Type.var () (v s)
              v1 s = Var.freshenId 1 (v s)
              tm :: Term.Term Symbol ()
              tm =
                Term.ann
                  ()
                  ( Term.ann
                      ()
                      (Term.nat () 42)
                      ( Type.introOuter () (v "a") $
                          Type.arrow () (tv "a") (tv "x")
                      )
                  )
                  (Type.forAll () (v "a") (tv "a"))
              tm' = Term.substTypeVar (v "x") (tv "a") tm
              expected =
                Term.ann
                  ()
                  ( Term.ann
                      ()
                      (Term.nat () 42)
                      ( Type.introOuter () (v1 "a") $
                          Type.arrow () (Type.var () $ v1 "a") (tv "a")
                      )
                  )
                  (Type.forAll () (v1 "a") (Type.var () $ v1 "a"))
          note $ show tm'
          note $ show expected
          expect $ tm == tm
          expect $ tm' == tm'
          expect $ tm' == expected
          ok,
        scope "Term.unhashComponent" $
          let h = Hash.fromByteString (encodeUtf8 "abcd")
              ref = R.Id h 0
              v1 = Var.unnamedRef @Symbol ref
              -- input component: `ref = \v1 -> ref`
              component = Map.singleton ref (Term.lam () ((), v1) (Term.refId () ref))
              component' = Term.unhashComponent component
              -- expected unhashed component: `v2 = \v1 -> v2`, where `v2 /= v1`,
              -- i.e. `v2` cannot be just `ref` converted to a ref-named variable,
              -- since that would collide with `v1`
              (v2, _) = component' ! ref
           in expect $ v2 /= v1
      ]
