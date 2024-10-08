module Unison.PrettyPrintEnv
  ( PrettyPrintEnv (..),
    patterns,
    patternName,
    terms,
    types,
    allTermNames,
    allTypeNames,
    termName,
    typeName,
    termNameOrHashOnly,
    typeNameOrHashOnly,
    termNameOrHashOnlyFq,
    typeNameOrHashOnlyFq,
    biasTo,
    labeledRefName,
    -- | Exported only for cases where the codebase's configured hash length is unavailable.
    todoHashLength,
    addFallback,
    empty,
  )
where

import Data.Ord (Down (Down))
import Data.Semigroup (Max (Max))
import Unison.ConstructorReference (ConstructorReference)
import Unison.ConstructorType qualified as CT
import Unison.HashQualified (HashQualified)
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Prelude hiding (empty)
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent

data PrettyPrintEnv = PrettyPrintEnv
  { -- names for terms, constructors, and requests; e.g. [(original name, relativized and/or suffixified pretty name)]
    termNames :: Referent -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)],
    -- names for types; e.g. [(original name, possibly suffixified name)]
    typeNames :: Reference -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
  }
  deriving stock (Generic)

allTermNames :: PrettyPrintEnv -> Referent -> [HQ'.HashQualified Name]
allTermNames ppe = fmap snd . termNames ppe

allTypeNames :: PrettyPrintEnv -> Reference -> [HQ'.HashQualified Name]
allTypeNames ppe = fmap snd . typeNames ppe

terms :: PrettyPrintEnv -> Referent -> Maybe (HQ'.HashQualified Name)
terms ppe = fmap snd . listToMaybe . termNames ppe

types :: PrettyPrintEnv -> Reference -> Maybe (HQ'.HashQualified Name)
types ppe = fmap snd . listToMaybe . typeNames ppe

termNameOrHashOnly :: PrettyPrintEnv -> Referent -> HQ.HashQualified Name
termNameOrHashOnly ppe r = maybe (HQ.fromReferent r) HQ'.toHQ $ terms ppe r

typeNameOrHashOnly :: PrettyPrintEnv -> Reference -> HQ.HashQualified Name
typeNameOrHashOnly ppe r = maybe (HQ.fromReference r) HQ'.toHQ $ types ppe r

-- Like 'termNameOrHashOnly' but returns the fully qualified name
termNameOrHashOnlyFq :: PrettyPrintEnv -> Referent -> HQ.HashQualified Name
termNameOrHashOnlyFq ppe r =
  maybe (HQ.fromReferent r) HQ'.toHQ . fmap fst . listToMaybe $ termNames ppe r

-- Like 'typeNameOrHashOnly' but returns the fully qualified name
typeNameOrHashOnlyFq :: PrettyPrintEnv -> Reference -> HQ.HashQualified Name
typeNameOrHashOnlyFq ppe r =
  maybe (HQ.fromReference r) HQ'.toHQ . fmap fst . listToMaybe $ typeNames ppe r

patterns :: PrettyPrintEnv -> ConstructorReference -> Maybe (HQ'.HashQualified Name)
patterns ppe r =
  terms ppe (Referent.Con r CT.Data)
    <|> terms ppe (Referent.Con r CT.Effect)

instance Show PrettyPrintEnv where
  show _ = "PrettyPrintEnv"

-- | Attempts to find a name in primary ppe, falls back to backup ppe only if no names are
-- found. Typically one can use this to shadow global or absolute names with names that are
-- within the current path.
addFallback :: PrettyPrintEnv -> PrettyPrintEnv -> PrettyPrintEnv
addFallback primary fallback =
  PrettyPrintEnv
    ( \r ->
        let primaryNames = termNames primary r
         in if null primaryNames
              then termNames fallback r
              else primaryNames
    )
    ( \r ->
        let primaryNames = typeNames primary r
         in if null primaryNames
              then typeNames fallback r
              else primaryNames
    )

-- todo: these need to be a dynamic length, but we need additional info
todoHashLength :: Int
todoHashLength = 10

termName :: PrettyPrintEnv -> Referent -> HashQualified Name
termName env r =
  case terms env r of
    Nothing -> HQ.take todoHashLength (HQ.fromReferent r)
    Just name -> HQ'.toHQ name

typeName :: PrettyPrintEnv -> Reference -> HashQualified Name
typeName env r =
  case types env r of
    Nothing -> HQ.take todoHashLength (HQ.fromReference r)
    Just name -> HQ'.toHQ name

-- | Get a name for a LabeledDependency from the PPE.
labeledRefName :: PrettyPrintEnv -> LabeledDependency -> HashQualified Name
labeledRefName ppe = \case
  LD.TermReferent ref -> termName ppe ref
  LD.TypeReference ref -> typeName ppe ref

patternName :: PrettyPrintEnv -> ConstructorReference -> HashQualified Name
patternName env r =
  case patterns env r of
    Just name -> HQ'.toHQ name
    Nothing -> HQ.take todoHashLength $ HQ.fromPattern r

empty :: PrettyPrintEnv
empty = PrettyPrintEnv mempty mempty

-- | Prefer names which share a common prefix with any provided target.
--
-- Results are sorted according to the longest common prefix found against ANY target.
biasTo :: [Name] -> PrettyPrintEnv -> PrettyPrintEnv
biasTo targets PrettyPrintEnv {termNames, typeNames} =
  PrettyPrintEnv
    { termNames = \r ->
        r
          & termNames
          & prioritizeBias targets,
      typeNames = \r ->
        r
          & typeNames
          & prioritizeBias targets
    }

-- | Prefer names which share a common prefix with any provided target.
--
-- Results are sorted according to the longest common prefix found against ANY target.
--
-- >>> prioritizeBias ["a.b", "x"] [(HQ'.unsafeFromText "q", ()), (HQ'.unsafeFromText "x.y", ()), (HQ'.unsafeFromText "a.b.c", ())]
-- [(a.b.c,()),(x.y,()),(q,())]
--
-- Sort is stable if there are no common prefixes
-- >>> prioritizeBias ["not-applicable"] [(HQ'.unsafeFromText "q", ()), (HQ'.unsafeFromText "a.b.c", ()), (HQ'.unsafeFromText "x", ())]
-- [(q,()),(a.b.c,()),(x,())]
prioritizeBias :: [Name] -> [(HQ'.HashQualified Name, a)] -> [(HQ'.HashQualified Name, a)]
prioritizeBias targets =
  sortOn \(fqn, _) ->
    targets
      & foldMap
        ( \target ->
            Just (Max (length $ Name.commonPrefix target (HQ'.toName fqn)))
        )
      & fromMaybe 0
      & Down -- Sort large common prefixes highest
