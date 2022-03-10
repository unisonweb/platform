{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Hashing.V2.DataDeclaration
  ( DataDeclaration (..),
    EffectDeclaration (..),
    Decl,
    Modifier (..),
    hashDecls,
  )
where

import Control.Lens (over, _3)
import Data.Bifunctor (first, second)
import qualified Data.Map as Map
import qualified Unison.ABT as ABT
import Unison.Hash (Hash)
import qualified Unison.Hashing.V2.ABT as ABT
import Unison.Hashing.V2.Reference (Reference)
import qualified Unison.Hashing.V2.Reference as Reference
import qualified Unison.Hashing.V2.Reference.Util as Reference.Util
import Unison.Hashing.V2.Tokenizable (Hashable1)
import qualified Unison.Hashing.V2.Tokenizable as Hashable
import Unison.Hashing.V2.Type (Type)
import qualified Unison.Hashing.V2.Type as Type
import qualified Unison.Name as Name
import qualified Unison.Names.ResolutionResult as Names
import Unison.Prelude
import Unison.Var (Var)
import Prelude hiding (cycle)

type Decl v a = Either (EffectDeclaration v a) (DataDeclaration v a)

data Modifier = Structural | Unique Text --  | Opaque (Set Reference)
  deriving (Eq, Ord, Show)

data DataDeclaration v a = DataDeclaration
  { modifier :: Modifier,
    annotation :: a,
    bound :: [v],
    constructors' :: [(a, v, Type v a)]
  }
  deriving (Functor)

newtype EffectDeclaration v a = EffectDeclaration
  { toDataDecl :: DataDeclaration v a
  }
  deriving (Functor)

constructorTypes :: DataDeclaration v a -> [Type v a]
constructorTypes = (snd <$>) . constructors

constructors :: DataDeclaration v a -> [(v, Type v a)]
constructors (DataDeclaration _ _ _ ctors) = [(v, t) | (_, v, t) <- ctors]

toABT :: ABT.Var v => DataDeclaration v () -> ABT.Term F v ()
toABT dd = ABT.tm $ Modified (modifier dd) dd'
  where
    dd' = ABT.absChain (bound dd) (ABT.tm (Constructors (ABT.transform Type <$> constructorTypes dd)))

-- Implementation detail of `hashDecls`, works with unannotated data decls
hashDecls0 :: (Eq v, ABT.Var v, Show v) => Map v (DataDeclaration v ()) -> [(v, Reference.Id)]
hashDecls0 decls =
  let abts = toABT <$> decls
      ref r = ABT.tm (Type (Type.Ref (Reference.DerivedId r)))
      cs = Reference.Util.hashComponents ref abts
   in [(v, r) | (v, (r, _)) <- Map.toList cs]

-- | compute the hashes of these user defined types and update any free vars
--   corresponding to these decls with the resulting hashes
--
--   data List a = Nil | Cons a (List a)
--   becomes something like
--   (List, #xyz, [forall a. #xyz a, forall a. a -> (#xyz a) -> (#xyz a)])
--
-- NOTE: technical limitation, this implementation gives diff results if ctors
-- have the same FQN as one of the types. TODO: assert this and bomb if not
-- satisfied, or else do local mangling and unmangling to ensure this doesn't
-- affect the hash.
hashDecls ::
  (Eq v, Var v, Show v) =>
  Map v (DataDeclaration v a) ->
  Names.ResolutionResult v a [(v, Reference.Id, DataDeclaration v a)]
hashDecls decls = do
  -- todo: make sure all other external references are resolved before calling this
  let varToRef = hashDecls0 (void <$> decls)
      varToRef' = second Reference.DerivedId <$> varToRef
      decls' = bindTypes <$> decls
      bindTypes dd = dd {constructors' = over _3 (Type.bindExternal varToRef') <$> constructors' dd}
      typeReferences = Map.fromList (first Name.unsafeFromVar <$> varToRef')
      -- normalize the order of the constructors based on a hash of their types
      sortCtors dd = dd {constructors' = sortOn hash3 $ constructors' dd}
      hash3 (_, _, typ) = ABT.hash typ :: Hash
  decls' <- fmap sortCtors <$> traverse (bindReferences mempty typeReferences) decls'
  pure [(v, r, dd) | (v, r) <- varToRef, Just dd <- [Map.lookup v decls']]

bindReferences ::
  Var v =>
  Set v ->
  Map Name.Name Reference ->
  DataDeclaration v a ->
  Names.ResolutionResult v a (DataDeclaration v a)
bindReferences keepFree names (DataDeclaration m a bound constructors) = do
  constructors <- for constructors $ \(a, v, ty) ->
    (a,v,) <$> Type.bindReferences keepFree names ty
  pure $ DataDeclaration m a bound constructors

data F a
  = Type (Type.F a)
  | LetRec [a] a
  | Constructors [a]
  | Modified Modifier a
  deriving (Functor, Foldable)

instance Hashable1 F where
  hash1 hashCycle hash e =
    let (tag, hashed) = (Hashable.Tag, Hashable.Hashed)
     in -- Note: start each layer with leading `2` byte, to avoid collisions with
        -- terms, which start each layer with leading `1`. See `Hashable1 Term.F`
        Hashable.accumulate $
          tag 2 : case e of
            Type t -> [tag 0, hashed $ Hashable.hash1 hashCycle hash t]
            LetRec bindings body ->
              let (hashes, hash') = hashCycle bindings
               in [tag 1] ++ map hashed hashes ++ [hashed $ hash' body]
            Constructors cs ->
              let (hashes, _) = hashCycle cs
               in tag 2 : map hashed hashes
            Modified m t ->
              [tag 3, Hashable.accumulateToken m, hashed $ hash t]

instance Hashable.Tokenizable Modifier where
  tokens Structural = [Hashable.Tag 0]
  tokens (Unique txt) = [Hashable.Tag 1, Hashable.Text txt]
