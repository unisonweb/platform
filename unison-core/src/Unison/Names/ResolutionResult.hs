module Unison.Names.ResolutionResult
  ( ResolutionError (..),
    ResolutionFailure (..),
    ResolutionResult,
    getAnnotation,
  )
where

import Unison.Name (Name)
import Unison.Names (Names)
import Unison.Prelude
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import Unison.HashQualified (HashQualified)

data ResolutionError ref
  = NotFound
  | -- Contains:
    --
    --   1. The namespace names
    --   2. The refs among those that we could be referring to
    --   3. The local names that we could be referring to
    --
    -- The size of set (2.) + the size of set (3.) is at least 2 (otherwise there wouldn't be any ambiguity).
    Ambiguous Names (Set ref) (Set Name)
  deriving (Eq, Ord, Show)

-- | ResolutionFailure represents the failure to resolve a given name.
data ResolutionFailure annotation
  = TypeResolutionFailure (HashQualified Name) annotation (ResolutionError TypeReference)
  | TermResolutionFailure (HashQualified Name) annotation (ResolutionError Referent)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

getAnnotation :: ResolutionFailure a -> a
getAnnotation = \case
  TypeResolutionFailure _ a _ -> a
  TermResolutionFailure _ a _ -> a

type ResolutionResult a r = Either (Seq (ResolutionFailure a)) r
