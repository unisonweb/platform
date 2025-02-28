{-# LANGUAGE DerivingVia #-}

module U.Codebase.Sqlite.Decl.Format where

import Data.Vector (Vector)
import U.Codebase.Decl (DeclR)
import U.Codebase.Reference (Reference')
import U.Codebase.Sqlite.DbId (ObjectId, TextId)
import U.Codebase.Sqlite.LocalIds (LocalDefnId, LocalIds', LocalTextId)
import U.Codebase.Sqlite.Symbol (Symbol)
import U.Codebase.Type qualified as Type
import U.Core.ABT qualified as ABT
import Unison.Hash32 (Hash32)
import Unison.Prelude

-- | Add new formats here
data DeclFormat' text defn = Decl (LocallyIndexedComponent' text defn)
  deriving (Show)

type DeclFormat = DeclFormat' TextId ObjectId

-- | A DeclFormat which uses hash references instead of database ids.
type HashDeclFormat = DeclFormat' Text Hash32

-- | V1: Decls included `Hash`es inline
--   V2: Instead of `Hash`, we use a smaller index.
type LocallyIndexedComponent =
  LocallyIndexedComponent' TextId ObjectId

newtype LocallyIndexedComponent' t d = LocallyIndexedComponent
  {unLocallyIndexedComponent :: Vector (LocalIds' t d, Decl Symbol)}
  deriving (Show)

type SyncDeclFormat =
  SyncDeclFormat' TextId ObjectId

data SyncDeclFormat' t d
  = SyncDecl (SyncLocallyIndexedComponent' t d)
  deriving stock (Eq, Show)

newtype SyncLocallyIndexedComponent' t d
  = SyncLocallyIndexedComponent (Vector (LocalIds' t d, ByteString))
  deriving stock (Eq, Show)

-- [OldDecl] ==map==> [NewDecl] ==number==> [(NewDecl, Int)] ==sort==> [(NewDecl, Int)] ==> permutation is map snd of that

-- type List a = Nil | Cons (List a)

-- unique type Thunk = Thunk (Int ->{MakeThunk} Int)
-- ability MakeThunk where go : (Int -> Int) -> Thunk

-- What mitchell thinks unhashComponent is doing:
--
--  Take a recursive type like
--
--     Fix \myself -> Alternatives [Nil, Cons a myself]
--
--  And write it with variables in place of recursive mentions like
--
--     (Var 1, Alternatives [Nil, Cons a (Var 1)]

-- can derive `original` from Hash + [OldDecl]
-- original :: Map Reference.Id (Decl v a)

-- named, rewritten_dependencies :: Map (Reference.Id {old}) (v, Decl v a {old pos in references})
-- named = Decl.unhashComponent original

-- Mapping from the sky: (Reference.Id -> Reference.Id)

-- rewritten_dependencies = replace_dependency_pos's skymap named

-- new_references :: Map v (Reference.Id {new}, DataDeclaration v a)
-- new_references = Unison.Hashing.V2.Convert.hashDecls $ Map.toList $ Foldable.toList rewritten_dependencies
-- hashDecls ::
--   Var v =>
--   Map v (Memory.DD.DataDeclaration v a) ->
--   ResolutionResult v a [(v, Memory.Reference.Id, Memory.DD.DataDeclaration v a)]

-- compute correspondence between `v`s in `fst <$> named` compared to `fst <$> new_references` to get a Reference.Id -> Reference.Id mapping
-- mitchell tapped out before understanding the following line
-- compute correspondence between constructors names & constructor indices in corresponding decls
-- submit/mappend these two correspondences to sky mapping

-- Swap the Reference positions according to our map of already computed swaps
-- Hydrate into the parser-typechecker version, get the new hash
-- reserialize it into the sqlite format
-- Compare the old and new sqlite versions to add those ConstructorID/Pos mappings to our context.

-- unrelated Q:
--   do we kinda have circular dependency issues here?
--   parser-typechecker depends on codebase2, but we are talking about doing things at the parser-typechecker level in this migration
--   answer: no

type Decl v = DeclR TypeRef v

type Type v = ABT.Term F v ()

type F = Type.F' TypeRef

type TypeRef = Reference' LocalTextId (Maybe LocalDefnId)
