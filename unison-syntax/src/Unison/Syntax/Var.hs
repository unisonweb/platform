module Unison.Syntax.Var
  ( namespaced,
    namespaced2,
  )
where

import Data.List.NonEmpty (pattern (:|))
import Data.List.NonEmpty qualified as List (NonEmpty)
import Unison.Name qualified as Name
import Unison.Prelude
import Unison.Syntax.Name qualified as Name
import Unison.Var (Var)

namespaced :: (Var v) => List.NonEmpty v -> v
namespaced (v :| vs) =
  Name.toVar (foldl' Name.joinDot (Name.unsafeParseVar v) (map Name.unsafeParseVar vs))

-- | Like 'namespaced', but for the common case that you have two vars to join.
namespaced2 :: (Var v) => v -> v -> v
namespaced2 v1 v2 =
  namespaced (v1 :| [v2])
