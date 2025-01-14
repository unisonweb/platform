module Unison.Codebase.CodeLookup where

import Control.Monad.Morph (MFunctor (..))
import Data.Set qualified as Set
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DD
import Unison.Prelude
import Unison.Reference qualified as Reference
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Util.Defns (Defns (..))
import Unison.Util.Set qualified as Set
import Unison.Var (Var)

data CodeLookup v m a = CodeLookup
  { getTerm :: Reference.Id -> m (Maybe (Term v a)),
    getTypeOfTerm :: Reference.Id -> m (Maybe (Type v a)),
    getTypeDeclaration :: Reference.Id -> m (Maybe (Decl v a))
  }

instance MFunctor (CodeLookup v) where
  hoist f (CodeLookup tm tmTyp tp) = CodeLookup (f . tm) (f . tmTyp) (f . tp)

instance (Ord v, Functor m) => Functor (CodeLookup v m) where
  fmap f cl = CodeLookup tm tmTyp ty
    where
      tm id = fmap (Term.amap f) <$> getTerm cl id
      ty id = fmap md <$> getTypeDeclaration cl id
      tmTyp id = (fmap . fmap) f <$> getTypeOfTerm cl id
      md (Left e) = Left (f <$> e)
      md (Right d) = Right (f <$> d)

instance (Monad m) => Semigroup (CodeLookup v m a) where
  c1 <> c2 = CodeLookup tm tmTyp ty
    where
      tm id = do
        o <- getTerm c1 id
        case o of Nothing -> getTerm c2 id; Just _ -> pure o
      tmTyp id = do
        o <- getTypeOfTerm c1 id
        case o of Nothing -> getTypeOfTerm c2 id; Just _ -> pure o
      ty id = do
        o <- getTypeDeclaration c1 id
        case o of Nothing -> getTypeDeclaration c2 id; Just _ -> pure o

instance (Monad m) => Monoid (CodeLookup v m a) where
  mempty = CodeLookup (const $ pure Nothing) (const $ pure Nothing) (const $ pure Nothing)

-- todo: can this be implemented in terms of TransitiveClosure.transitiveClosure?
-- todo: add some tests on this guy?
transitiveDependencies ::
  (Monad m, Var v) =>
  CodeLookup v m a ->
  Set Reference.Id ->
  Reference.Id ->
  m (Set Reference.Id)
transitiveDependencies code seen0 rid =
  if Set.member rid seen0
    then pure seen0
    else
      let seen = Set.insert rid seen0
          getIds = Set.mapMaybe Reference.toId
       in getTerm code rid >>= \case
            Just t ->
              foldM (transitiveDependencies code) seen (getIds $ let deps = Term.dependencies t in deps.terms <> deps.types)
            Nothing ->
              getTypeDeclaration code rid >>= \case
                Nothing -> pure seen
                Just (Left ed) ->
                  foldM
                    (transitiveDependencies code)
                    seen
                    (getIds $ DD.typeDependencies (DD.toDataDecl ed))
                Just (Right dd) ->
                  foldM
                    (transitiveDependencies code)
                    seen
                    (getIds $ DD.typeDependencies dd)
