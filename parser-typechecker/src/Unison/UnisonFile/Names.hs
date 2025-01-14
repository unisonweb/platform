module Unison.UnisonFile.Names
  ( addNamesFromTypeCheckedUnisonFile,
    environmentFor,
    toNames,
    toTermAndWatchNames,
    typecheckedToNames,
  )
where

import Control.Lens (_1)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.DataDeclaration (DataDeclaration, EffectDeclaration (..))
import Unison.DataDeclaration qualified as DD
import Unison.DataDeclaration.Names qualified as DD.Names
import Unison.Hashing.V2.Convert qualified as Hashing
import Unison.Names (Names (..))
import Unison.Names qualified as Names
import Unison.Names.ResolutionResult qualified as Names
import Unison.Prelude
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Syntax.Name qualified as Name
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Env (Env (..))
import Unison.UnisonFile.Error (Error (DupDataAndAbility, UnknownType))
import Unison.UnisonFile.Type (TypecheckedUnisonFile, UnisonFile)
import Unison.Util.Relation qualified as Relation
import Unison.Var (Var)
import Unison.WatchKind qualified as WK

toNames :: (Var v) => UnisonFile v a -> Names
toNames uf = datas <> effects
  where
    datas = foldMap (DD.Names.dataDeclToNames' Name.unsafeParseVar) (Map.toList (UF.dataDeclarationsId uf))
    effects = foldMap (DD.Names.effectDeclToNames' Name.unsafeParseVar) (Map.toList (UF.effectDeclarationsId uf))

-- | The set of all term and test watch names. No constructors.
toTermAndWatchNames :: (Var v) => UnisonFile v a -> Set v
toTermAndWatchNames uf =
  Map.keysSet uf.terms
    <> foldMap
      ( \case
          (WK.TestWatch, xs) -> Set.fromList (map (view _1) xs)
          _ -> Set.empty
      )
      (Map.toList uf.watches)

typecheckedToNames :: (Var v) => TypecheckedUnisonFile v a -> Names
typecheckedToNames uf = Names (terms <> ctors) types
  where
    terms =
      Relation.fromList
        [ (Name.unsafeParseVar v, Referent.Ref r)
          | (v, (_a, r, wk, _, _)) <- Map.toList $ UF.hashTerms uf,
            wk == Nothing || wk == Just WK.TestWatch
        ]
    types =
      Relation.fromList
        [ (Name.unsafeParseVar v, r)
          | (v, r) <-
              Map.toList $
                fmap fst (UF.dataDeclarations' uf)
                  <> fmap fst (UF.effectDeclarations' uf)
        ]
    ctors =
      Relation.fromMap
        . Map.mapKeys Name.unsafeParseVar
        . fmap (fmap Reference.DerivedId)
        . UF.hashConstructors
        $ uf

addNamesFromTypeCheckedUnisonFile :: (Var v) => TypecheckedUnisonFile v a -> Names -> Names
addNamesFromTypeCheckedUnisonFile unisonFile names = Names.shadowing (typecheckedToNames unisonFile) names

-- This function computes hashes for data and effect declarations, and
-- also returns a function for resolving strings to (Reference, ConstructorId)
-- for parsing of pattern matching
--
-- If there are duplicate declarations, the duplicated names are returned on the
-- left.
environmentFor ::
  forall v a.
  (Var v) =>
  Names ->
  Map v (DataDeclaration v a) ->
  Map v (EffectDeclaration v a) ->
  Names.ResolutionResult a (Either [Error v a] (Env v a))
environmentFor names dataDecls0 effectDecls0 = do
  let locallyBoundTypes = Map.keysSet dataDecls0 <> Map.keysSet effectDecls0

  -- data decls and effect decls may reference each other, and thus must be hashed together
  dataDecls :: Map v (DataDeclaration v a) <-
    traverse (DD.Names.bindNames Name.unsafeParseVar Name.toVar locallyBoundTypes names) dataDecls0
  effectDecls :: Map v (EffectDeclaration v a) <-
    traverse (DD.withEffectDeclM (DD.Names.bindNames Name.unsafeParseVar Name.toVar locallyBoundTypes names)) effectDecls0

  let allDecls0 :: Map v (DataDeclaration v a)
      allDecls0 = Map.union dataDecls (toDataDecl <$> effectDecls)
  hashDecls' :: [(v, Reference.Id, DataDeclaration v a)] <- Hashing.hashDataDecls allDecls0
  -- then we have to pick out the dataDecls from the effectDecls
  let allDecls = Map.fromList [(v, (r, de)) | (v, r, de) <- hashDecls']
      dataDecls' = Map.difference allDecls effectDecls
      effectDecls' = second EffectDeclaration <$> Map.difference allDecls dataDecls
      -- ctor and effect terms
      ctors = foldMap (DD.Names.dataDeclToNames' Name.unsafeParseVar) (Map.toList dataDecls')
      effects = foldMap (DD.Names.effectDeclToNames' Name.unsafeParseVar) (Map.toList effectDecls')
      names' = ctors <> effects
      overlaps =
        let w v dd (toDataDecl -> ed) = DupDataAndAbility v (DD.annotation dd) (DD.annotation ed)
         in Map.elems $ Map.intersectionWithKey w dataDecls effectDecls
        where

      okVars = Map.keysSet allDecls0
      unknownTypeRefs =
        Map.elems allDecls0 >>= \dd ->
          let cts = DD.constructorTypes dd
           in cts >>= \ct ->
                [ UnknownType v a | (v, a) <- ABT.freeVarOccurrences mempty ct, not (Set.member v okVars)
                ]
  pure $
    if null overlaps && null unknownTypeRefs
      then pure $ Env dataDecls' effectDecls' names'
      else Left (unknownTypeRefs ++ overlaps)
