-- | "Unison.Codebase.Branch" tests.
module Unison.Test.Codebase.Branch
  ( test,
  )
where

import Data.Functor.Identity
import Data.Map qualified as Map
import Data.Set qualified as Set
import EasyTest
import Unison.Codebase.Branch (Branch (Branch), Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Causal qualified as Causal
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Util.Relation qualified as Relation
import Unison.Util.Star3 qualified as Star3

test :: Test ()
test =
  (scope "codebase.branch" . tests)
    [ scope "branch0" (tests branch0Tests)
    ]

branch0Tests :: [Test ()]
branch0Tests =
  [ scope "regression-2564" do
      let dummy :: Reference =
            Reference.Builtin "foo"
      let -- b
          b0 :: Branch0 Identity =
            Branch.branch0
              mempty
              (Star3.fromList [(dummy, "b", dummy, (dummy, dummy))])
              Map.empty
              Map.empty
      let -- a.b
          -- b
          b1 :: Branch0 Identity =
            Branch.branch0
              mempty
              (Star3.fromList [(dummy, "b", dummy, (dummy, dummy))])
              (Map.singleton "a" (Branch (Causal.one b0)))
              Map.empty

      let -- b.a.b
          -- b.b
          b2 :: Branch0 Identity =
            Branch.branch0
              mempty
              mempty
              (Map.singleton "b" (Branch (Causal.one b1)))
              Map.empty

      expect (Set.valid (Relation.ran (Branch.deepTypes b2)))
  ]
