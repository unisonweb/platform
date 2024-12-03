module Unison.Type.Names
  ( bindNames,
  )
where

import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Names.ResolutionResult qualified as Names
import Unison.Names.ResolvesTo (ResolvesTo (..), partitionResolutions)
import Unison.Prelude
import Unison.Reference (TypeReference)
import Unison.Type
import Unison.Type qualified as Type
import Unison.Util.List qualified as List
import Unison.Var (Var)

bindNames ::
  forall a v.
  (Var v) =>
  (v -> Name) ->
  (Name -> v) ->
  Set v ->
  Names ->
  Type v a ->
  Names.ResolutionResult a (Type v a)
bindNames unsafeVarToName nameToVar localVars namespace =
  -- type is bound here because the where-clause binds a data structure that we only want to compute once, then share
  -- across all calls to `bindNames` with different types
  \ty ->
    let -- Identify the unresolved variables in the type: those whose names aren't an *exact* match for some locally-bound
        -- type.
        --
        -- For example:
        --
        --   type Foo.Bar = ...
        --   type Baz.Qux = ...
        --   type Whatever = Whatever Foo.Bar Qux
        --                            ^^^^^^^ ^^^
        --                               |    this variable *is* unresolved: it doesn't match any locally-bound type exactly
        --                               |
        --                            this variable is *not* unresolved: it matches locally-bound `Foo.Bar` exactly
        unresolvedVars :: [(v, a)]
        unresolvedVars =
          ABT.freeVarOccurrences localVars ty

        okTy :: (v, a) -> Names.ResolutionResult a (v, ResolvesTo TypeReference)
        okTy (v, a) =
          case Set.size matches of
            1 -> good (Set.findMin matches)
            0 -> bad Names.NotFound
            _ ->
              let (namespaceMatches, localMatches) =
                    matches
                      & Set.toList
                      & map \case
                        ResolvesToNamespace ref -> Left ref
                        ResolvesToLocal name -> Right name
                      & partitionEithers
                      & bimap Set.fromList Set.fromList
               in bad (Names.Ambiguous namespace namespaceMatches localMatches)
          where
            matches :: Set (ResolvesTo TypeReference)
            matches =
              resolveTypeName (unsafeVarToName v)

            bad = Left . Seq.singleton . Names.TypeResolutionFailure (HQ.NameOnly (unsafeVarToName v)) a
            good = Right . (v,)
     in List.validate okTy unresolvedVars <&> \resolutions ->
          let (namespaceResolutions, localResolutions) = partitionResolutions resolutions
           in ty
                -- Apply namespace resolutions (replacing "Foo" with #Foo where "Foo" refers to namespace)
                & bindExternal namespaceResolutions
                -- Apply local resolutions (replacing "Foo" with "Full.Name.Foo" where "Full.Name.Foo" is in local vars)
                & ABT.substsInheritAnnotation [(v, Type.var () (nameToVar name)) | (v, name) <- localResolutions]
                -- Clean up ability lists again – we might have something to de-dupe after resolution
                & Type.cleanupAbilityLists
  where
    resolveTypeName :: Name -> Set (ResolvesTo TypeReference)
    resolveTypeName =
      Names.resolveName (Names.types namespace) (Set.map unsafeVarToName localVars)
