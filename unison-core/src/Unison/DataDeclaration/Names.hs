{-# LANGUAGE RecordWildCards #-}

module Unison.DataDeclaration.Names (bindNames, dataDeclToNames', effectDeclToNames') where

import Data.Set qualified as Set
import Data.Map qualified as Map
import Unison.ABT qualified as ABT
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType qualified as CT
import Unison.DataDeclaration (DataDeclaration (DataDeclaration), EffectDeclaration)
import Unison.DataDeclaration qualified as DD
import Unison.Name qualified as Name
import Unison.Names (Names (Names))
import Unison.Names.ResolutionResult qualified as Names
import Unison.Prelude
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Type qualified as Type
import Unison.Type.Names qualified as Type.Names
import Unison.Util.Relation qualified as Rel
import Unison.Var (Var)
import Prelude hiding (cycle)

-- implementation of dataDeclToNames and effectDeclToNames
toNames :: (Var v) => (v -> Name.Name) -> CT.ConstructorType -> v -> Reference.Id -> DataDeclaration v a -> Names
toNames varToName ct typeSymbol (Reference.DerivedId -> r) dd =
  -- constructor names
  foldMap names (DD.constructorVars dd `zip` [0 ..])
    -- name of the type itself
    <> Names mempty (Rel.singleton (varToName typeSymbol) r)
  where
    names (ctor, i) =
      Names (Rel.singleton (varToName ctor) (Referent.Con (ConstructorReference r i) ct)) mempty

dataDeclToNames :: (Var v) => (v -> Name.Name) -> v -> Reference.Id -> DataDeclaration v a -> Names
dataDeclToNames varToName = toNames varToName CT.Data

effectDeclToNames :: (Var v) => (v -> Name.Name) -> v -> Reference.Id -> EffectDeclaration v a -> Names
effectDeclToNames varToName typeSymbol r ed = toNames varToName CT.Effect typeSymbol r $ DD.toDataDecl ed

dataDeclToNames' :: (Var v) => (v -> Name.Name) -> (v, (Reference.Id, DataDeclaration v a)) -> Names
dataDeclToNames' varToName (v, (r, d)) = dataDeclToNames varToName v r d

effectDeclToNames' :: (Var v) => (v -> Name.Name) -> (v, (Reference.Id, EffectDeclaration v a)) -> Names
effectDeclToNames' varToName (v, (r, d)) = effectDeclToNames varToName v r d

-- | @bindNames varToName localNames names decl@ adjusts each constructor type in @decl@ by:
--
--   1. Expanding unique suffix references to locally-defined things to their full name.
--   2. Replacing each free variable (but not to locally-defined things!) with the corresponding reference.
--
-- For example, consider the following Unison file.
--
--     unique type Foo.Bar.Baz = Qux Nat | Waffle Int
--
--     unique type Wombat = WomOne Baz | WomTwo Whamlet Nat
--
-- When processing decl Wombat, step (1) would replace "Baz" with "Foo.Bar.Baz":
--
--     unique type Wombat = WomOne Foo.Bar.Baz | WomTwo Whamlet Nat
--
-- And step (2) would bind non-local variables to type references:
--
--     unique type Wombat = WomOne Foo.Bar.Baz | WomTwo #whamlet ##Nat
bindNames ::
  (Var v) =>
  (v -> Name.Name) ->
  Map v v ->
  Names ->
  DataDeclaration v a ->
  Names.ResolutionResult v a (DataDeclaration v a)
bindNames varToName localNames names (DataDeclaration m a bound constructors) = do
  constructors <- for constructors \(a, v, ty) ->
    (a,v,) <$> Type.Names.bindNames varToName keepFree names (ABT.substsInheritAnnotation subs ty)
  pure $ DataDeclaration m a bound constructors
  where
    keepFree = Set.fromList (Map.elems localNames)
    subs = Map.toList $ Map.map (Type.var ()) localNames
