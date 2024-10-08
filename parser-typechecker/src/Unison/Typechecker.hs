{-# LANGUAGE TemplateHaskell #-}

-- | This module is the primary interface to the Unison typechecker
module Unison.Typechecker
  ( synthesize,
    synthesizeAndResolve,
    isEqual,
    isSubtype,
    fitsScheme,
    Env (..),
    Notes (..),
    Resolution (..),
    NamedReference (..),
    Context.PatternMatchCoverageCheckAndKindInferenceSwitch (..),
  )
where

import Control.Lens
import Control.Monad.Fail (fail)
import Control.Monad.State (State, StateT, execState, get, modify)
import Control.Monad.Writer
import Data.Foldable
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as NESeq (toSeq)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Tuple qualified as Tuple
import Unison.ABT qualified as ABT
import Unison.Blank qualified as B
import Unison.Codebase.BuiltinAnnotation (BuiltinAnnotation)
import Unison.Name qualified as Name
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.Result (Result, ResultT, runResultT, pattern Result)
import Unison.Result qualified as Result
import Unison.Syntax.Name qualified as Name (unsafeParseText, unsafeParseVar)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.Typechecker.TypeVar qualified as TypeVar
import Unison.Util.List (uniqueBy)
import Unison.Var (Var)
import Unison.Var qualified as Var

data Notes v loc = Notes
  { bugs :: Seq (Context.CompilerBug v loc),
    errors :: Seq (Context.ErrorNote v loc),
    infos :: Seq (Context.InfoNote v loc)
  }
  deriving (Show)

instance Semigroup (Notes v loc) where
  Notes bs es is <> Notes bs' es' is' = Notes (bs <> bs') (es <> es') (is <> is')

instance Monoid (Notes v loc) where
  mempty = Notes mempty mempty mempty

convertResult :: Context.Result v loc a -> Result (Notes v loc) a
convertResult = \case
  Context.Success is a -> Result (Notes mempty mempty is) (Just a)
  Context.TypeError es is -> Result (Notes mempty (NESeq.toSeq es) is) Nothing
  Context.CompilerBug bug es is -> Result (Notes (Seq.singleton bug) es is) Nothing

data NamedReference v loc = NamedReference
  { fqn :: Name.Name,
    fqnType :: Type v loc,
    replacement :: Context.Replacement v
  }
  deriving stock (Show)

data Env v loc = Env
  { ambientAbilities :: [Type v loc],
    typeLookup :: TL.TypeLookup v loc,
    -- TDNR environment - maps short names like `+` to fully-qualified
    -- lists of named references whose full name matches the short name
    -- Example: `+` maps to [Nat.+, Float.+, Int.+]
    --
    -- This mapping is populated before typechecking with as few entries
    -- as are needed to help resolve variables needing TDNR in the file.
    --
    -- - Left means a term in the file (for which we don't have a type before typechecking)
    -- - Right means a term/constructor in the namespace, or a constructor in the file (for which we do have a type
    --   before typechecking)
    termsByShortname :: Map Name.Name [Either Name.Name (NamedReference v loc)],
    topLevelComponents :: Map Name.Name (NamedReference v loc)
  }
  deriving stock (Generic)

-- | Infer the type of a 'Unison.Term', using
-- a function to resolve the type of @Ref@ constructors
-- contained in that term.
synthesize ::
  (Monad f, Var v, BuiltinAnnotation loc, Ord loc, Show loc) =>
  PrettyPrintEnv ->
  Context.PatternMatchCoverageCheckAndKindInferenceSwitch ->
  Env v loc ->
  Term v loc ->
  ResultT (Notes v loc) f (Type v loc)
synthesize ppe pmccSwitch env t =
  let result =
        convertResult $
          Context.synthesizeClosed
            ppe
            pmccSwitch
            (TypeVar.liftType <$> env.ambientAbilities)
            env.typeLookup
            (TypeVar.liftTerm t)
   in Result.hoist (pure . runIdentity) $ fmap TypeVar.lowerType result

isSubtype :: (Var v) => Type v loc -> Type v loc -> Bool
isSubtype t1 t2 =
  handleCompilerBug (Context.isSubtype (tvar $ void t1) (tvar $ void t2))
  where
    tvar = TypeVar.liftType

handleCompilerBug :: (Var v) => Either (Context.CompilerBug v ()) a -> a
handleCompilerBug = \case
  Left bug -> error $ "compiler bug encountered: " ++ show bug
  Right b -> b

-- | Similar to 'isSubtype' but treats @t2@ as a scheme where the
-- outermost variables are existential rather than universal.
--
-- For example:
-- @
-- let
--   lhs = Unison.Type.ref () (Unison.Builtin.Decls.unitRef)
--   rhs = Unison.Type.forall () (Unison.Var.named "x") (Unison.Type.var () (Unison.Var.named "x"))
-- in fitsScheme @Symbol lhs rhs
-- @
-- is @True@ although the lhs is not a subtype of the rhs.
--
-- 'fitsScheme' is used to check that runnable types are a subtype of
-- @
-- exists x. '{IO, Exception} x
-- @
fitsScheme :: (Var v) => Type v loc -> Type v loc -> Bool
fitsScheme t1 t2 = handleCompilerBug (Context.fitsScheme (tvar $ void t1) (tvar $ void t2))
  where
    tvar = TypeVar.liftType

isEqual :: (Var v) => Type v loc -> Type v loc -> Bool
isEqual t1 t2 = isSubtype t1 t2 && isSubtype t2 t1

type TDNR f v loc a =
  StateT (Term v loc) (ResultT (Notes v loc) f) a

data Resolution v loc = Resolution
  { resolvedName :: Text,
    inferredType :: Context.Type v loc,
    resolvedLoc :: loc,
    v :: v,
    suggestions :: [Context.Suggestion v loc]
  }

-- | Infer the type of a 'Unison.Term', using type-directed name resolution
-- to attempt to resolve unknown symbols.
synthesizeAndResolve ::
  (Monad f, Var v, Monoid loc, BuiltinAnnotation loc, Ord loc, Show loc) => PrettyPrintEnv -> Env v loc -> TDNR f v loc (Type v loc)
synthesizeAndResolve ppe env = do
  tm <- get
  (tp, notes) <-
    listen . lift $
      synthesize
        ppe
        Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Enabled
        env
        tm
  typeDirectedNameResolution ppe notes tp env

compilerBug :: Context.CompilerBug v loc -> Result (Notes v loc) ()
compilerBug bug = do
  tell $ Notes (Seq.singleton bug) mempty mempty
  Control.Monad.Fail.fail ""

typeError :: Context.ErrorNote v loc -> Result (Notes v loc) ()
typeError note = do
  tell $ Notes mempty (Seq.singleton note) mempty
  Control.Monad.Fail.fail ""

btw :: (Monad f) => Context.InfoNote v loc -> ResultT (Notes v loc) f ()
btw note = tell $ Notes mempty mempty (Seq.singleton note)

liftResult :: (Monad f) => Result (Notes v loc) a -> TDNR f v loc a
liftResult = lift . MaybeT . WriterT . pure . runIdentity . runResultT

-- Resolve "solved blanks". If a solved blank's type and name matches the type
-- and unqualified name of a symbol that isn't imported, provide a note
-- suggesting the import. If the blank is ambiguous and only one typechecks, use
-- that one.  Otherwise, provide an unknown symbol error to the user.
-- The cases we consider are:
-- 1. There exist names that match and their types match too. Tell the user
--    the fully qualified names of these terms, and their types.
-- 2. There's more than one name that matches,
--    but only one that typechecks. Substitute that one into the code.
-- 3. No match at all. Throw an unresolved symbol at the user.
typeDirectedNameResolution ::
  forall v loc f.
  (Monad f, Var v, BuiltinAnnotation loc, Ord loc, Monoid loc, Show loc) =>
  PrettyPrintEnv ->
  Notes v loc ->
  Type v loc ->
  Env v loc ->
  TDNR f v loc (Type v loc)
typeDirectedNameResolution ppe oldNotes oldType env = do
  -- Add typed components (local definitions) to the TDNR environment.
  let tdnrEnv = execState (traverse_ addTypedComponent $ infos oldNotes) env
  -- Resolve blanks in the notes and generate some resolutions
  resolutions <-
    liftResult . traverse (resolveNote tdnrEnv) . toList $
      infos oldNotes
  case catMaybes resolutions of
    [] -> pure oldType
    resolutions -> do
      substituted <- traverse substSuggestion resolutions
      case or substituted of
        True -> synthesizeAndResolve ppe tdnrEnv
        False -> do
          -- The type hasn't changed
          liftResult $ suggest resolutions
          pure oldType
  where
    addTypedComponent :: Context.InfoNote v loc -> State (Env v loc) ()
    addTypedComponent (Context.TopLevelComponent vtts) =
      for_ vtts \(v, typ, _) ->
        let name = Name.unsafeParseVar (Var.reset v)
         in #topLevelComponents %= Map.insert name (NamedReference name typ (Context.ReplacementVar v))
    addTypedComponent _ = pure ()

    suggest :: [Resolution v loc] -> Result (Notes v loc) ()
    suggest =
      traverse_ \(Resolution name inferredType loc v suggestions) ->
        typeError $
          Context.ErrorNote
            { cause = Context.UnknownTerm loc (suggestedVar v name) (dedupe suggestions) inferredType,
              path = Seq.empty
            }

    guard x a = if x then Just a else Nothing

    suggestedVar :: (Var v) => v -> Text -> v
    suggestedVar v name =
      case Var.typeOf v of
        Var.MissingResult -> v
        _ -> Var.named name

    extractSubstitution :: [Context.Suggestion v loc] -> Maybe (Context.Replacement v)
    extractSubstitution suggestions =
      let groupedByName :: [([Name.Name], Context.Replacement v)] =
            map Tuple.swap
              . Map.toList
              . fmap Set.toList
              . foldl'
                ( \b Context.Suggestion {suggestionName, suggestionReplacement} ->
                    Map.insertWith
                      Set.union
                      suggestionReplacement
                      (Set.singleton suggestionName)
                      b
                )
                Map.empty
              $ filter Context.isExact suggestions
          matches :: Set (Context.Replacement v) = Name.preferShallowLibDepth groupedByName
       in case toList matches of
            [x] -> Just x
            _ -> Nothing

    substSuggestion :: Resolution v loc -> TDNR f v loc Bool
    substSuggestion (Resolution name _ loc v (extractSubstitution -> Just replacement)) = do
      modify (substBlank name loc solved)
      lift . btw $ Context.Decision (suggestedVar v name) loc solved
      pure True
      where
        solved =
          case replacement of
            Context.ReplacementRef ref -> Term.fromReferent loc ref
            Context.ReplacementVar var -> Term.var loc var
    substSuggestion _ = pure False

    -- Resolve a `Blank` to a term
    substBlank :: Text -> loc -> Term v loc -> Term v loc -> Term v loc
    substBlank s a r = ABT.visitPure go
      where
        go t = guard (ABT.annotation t == a) $ ABT.visitPure resolve t
        resolve (Term.Blank' (B.Recorded (B.Resolve loc name)))
          | name == Text.unpack s = Just (loc <$ r)
        resolve _ = Nothing

    -- Returns Nothing for irrelevant notes
    resolveNote ::
      Env v loc ->
      Context.InfoNote v loc ->
      Result (Notes v loc) (Maybe (Resolution v loc))
    resolveNote env = \case
      Context.SolvedBlank (B.Resolve loc str) v it -> do
        let shortname = Name.unsafeParseText (Text.pack str)
            matches =
              env.termsByShortname
                & Map.findWithDefault [] shortname
                & mapMaybe \case
                  Left longname -> Map.lookup longname env.topLevelComponents
                  Right namedRef -> Just namedRef
        suggestions <- wither (resolve it) matches
        pure $
          Just
            Resolution
              { resolvedName = Text.pack str,
                inferredType = it,
                resolvedLoc = loc,
                v,
                suggestions
              }
      -- Solve the case where we have a placeholder for a missing result
      -- at the end of a block. This is always an error.
      Context.SolvedBlank (B.MissingResultPlaceholder loc) v it ->
        pure . Just $ Resolution "_" it loc v []
      note -> do
        btw note
        pure Nothing

    dedupe :: [Context.Suggestion v loc] -> [Context.Suggestion v loc]
    dedupe =
      uniqueBy Context.suggestionReplacement

    resolve ::
      Context.Type v loc ->
      NamedReference v loc ->
      Result (Notes v loc) (Maybe (Context.Suggestion v loc))
    resolve inferredType (NamedReference fqn foundType replace) =
      -- We found a name that matches. See if the type matches too.
      case Context.isSubtype (TypeVar.liftType foundType) (Context.relax inferredType) of
        Left bug -> Nothing <$ compilerBug bug
        -- Suggest the import if the type matches.
        Right b ->
          pure . Just $
            Context.Suggestion
              fqn
              (TypeVar.liftType foundType)
              replace
              (if b then Context.Exact else Context.WrongType)
