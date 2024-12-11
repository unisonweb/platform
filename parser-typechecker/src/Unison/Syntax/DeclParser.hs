module Unison.Syntax.DeclParser
  ( declarations,
  )
where

import Control.Lens
import Control.Monad.Reader (MonadReader (..))
import Data.List.NonEmpty (pattern (:|))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Text.Megaparsec qualified as P
import Unison.ABT qualified as ABT
import Unison.DataDeclaration (DataDeclaration, EffectDeclaration)
import Unison.DataDeclaration qualified as DD
import Unison.Name qualified as Name
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Syntax.Lexer qualified as L
import Unison.Syntax.Name qualified as Name (toText, unsafeParseVar)
import Unison.Syntax.Parser
import Unison.Syntax.TermParser qualified as TermParser
import Unison.Syntax.TypeParser qualified as TypeParser
import Unison.Syntax.Var qualified as Var (namespaced)
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Var (Var)
import Unison.Var qualified as Var (name, named)
import Prelude hiding (readFile)

-- The parsed form of record accessors, as in:
--
-- type Additive a = { zero : a, (+) : a -> a -> a }
--
-- The `Token v` is the variable name and location (here `zero` and `(+)`) of
-- each field, and the type is the type of that field
type Accessors v = [(L.Token v, [(L.Token v, Type v Ann)])]

declarations ::
  (Monad m, Var v) =>
  P
    v
    m
    ( Map v (DataDeclaration v Ann),
      Map v (EffectDeclaration v Ann),
      Accessors v
    )
declarations = do
  declarations <- many $ declaration <* optional semi
  let (dataDecls0, effectDecls) = partitionEithers declarations
      dataDecls = [(a, b) | (a, b, _) <- dataDecls0]
      multimap :: (Ord k) => [(k, v)] -> Map k [v]
      multimap = foldl' mi Map.empty
      mi m (k, v) = Map.insertWith (++) k [v] m
      mds = multimap dataDecls
      mes = multimap effectDecls
      mdsBad = Map.filter (\xs -> length xs /= 1) mds
      mesBad = Map.filter (\xs -> length xs /= 1) mes
  if Map.null mdsBad && Map.null mesBad
    then
      pure
        ( Map.fromList dataDecls,
          Map.fromList effectDecls,
          join . map (view _3) $ dataDecls0
        )
    else
      P.customFailure . DuplicateTypeNames $
        [(v, DD.annotation <$> ds) | (v, ds) <- Map.toList mdsBad]
          <> [(v, DD.annotation . DD.toDataDecl <$> es) | (v, es) <- Map.toList mesBad]

-- | When we first walk over the modifier, it may be a `unique`, in which case we want to use a function in the parsing
-- environment to map the type's name (which we haven't parsed yet) to a GUID to reuse (if any).
--
-- So, we parse into this temporary "unresolved modifier" type, which is soon resolved to a real Modifier once we know
-- the type name.
data UnresolvedModifier
  = UnresolvedModifier'Structural
  | UnresolvedModifier'UniqueWithGuid !Text
  | UnresolvedModifier'UniqueWithoutGuid

resolveUnresolvedModifier :: (Monad m, Var v) => L.Token UnresolvedModifier -> v -> P v m (L.Token DD.Modifier)
resolveUnresolvedModifier unresolvedModifier var =
  case L.payload unresolvedModifier of
    UnresolvedModifier'Structural -> pure (DD.Structural <$ unresolvedModifier)
    UnresolvedModifier'UniqueWithGuid guid -> pure (DD.Unique guid <$ unresolvedModifier)
    UnresolvedModifier'UniqueWithoutGuid -> do
      unique <- resolveUniqueModifier var
      pure $ unique <$ unresolvedModifier

resolveUniqueModifier :: (Monad m, Var v) => v -> P v m DD.Modifier
resolveUniqueModifier var = do
  env <- ask
  guid <-
    lift (lift (env.uniqueTypeGuid (Name.unsafeParseVar var))) >>= \case
      Nothing -> uniqueName 32
      Just guid -> pure guid
  pure (DD.Unique guid)

-- unique[someguid] type Blah = ...
modifier :: (Monad m, Var v) => P v m (Maybe (L.Token UnresolvedModifier))
modifier = do
  optional (unique <|> structural)
  where
    unique = do
      tok <- openBlockWith "unique"
      optional (openBlockWith "[" *> importWordyId <* closeBlock) >>= \case
        Nothing -> pure (UnresolvedModifier'UniqueWithoutGuid <$ tok)
        Just guid -> pure (UnresolvedModifier'UniqueWithGuid (Name.toText (L.payload guid)) <$ tok)
    structural = do
      tok <- openBlockWith "structural"
      pure (UnresolvedModifier'Structural <$ tok)

declaration ::
  (Monad m, Var v) =>
  P
    v
    m
    ( Either
        (v, DataDeclaration v Ann, Accessors v)
        (v, EffectDeclaration v Ann)
    )
declaration = do
  mod <- modifier
  fmap Right (effectDeclaration mod) <|> fmap Left (dataDeclaration mod)

dataDeclaration ::
  forall m v.
  (Monad m, Var v) =>
  Maybe (L.Token UnresolvedModifier) ->
  P v m (v, DataDeclaration v Ann, Accessors v)
dataDeclaration maybeUnresolvedModifier = do
  typeToken <- fmap void (reserved "type") <|> openBlockWith "type"
  (name, typeArgs) <-
    (,)
      <$> TermParser.verifyRelativeVarName prefixDefinitionName
      <*> many (TermParser.verifyRelativeVarName prefixDefinitionName)
  let typeArgVs = L.payload <$> typeArgs
  eq <- reserved "="
  let -- go gives the type of the constructor, given the types of
      -- the constructor arguments, e.g. Cons becomes forall a . a -> List a -> List a
      go :: L.Token v -> [Type v Ann] -> (Ann {- Ann spanning the constructor and its args -}, (Ann, v, Type v Ann))
      go ctorName ctorArgs =
        let arrow i o = Type.arrow (ann i <> ann o) i o
            app f arg = Type.app (ann f <> ann arg) f arg
            -- ctorReturnType e.g. `Optional a`
            ctorReturnType = foldl' app (tok Type.var name) (tok Type.var <$> typeArgs)
            -- ctorType e.g. `a -> Optional a`
            --    or just `Optional a` in the case of `None`
            ctorType = foldr arrow ctorReturnType ctorArgs
            ctorAnn = ann ctorName <> maybe (ann ctorName) ann (lastMay ctorArgs)
         in ( ctorAnn,
              ( ann ctorName,
                Var.namespaced (L.payload name :| [L.payload ctorName]),
                Type.foralls ctorAnn typeArgVs ctorType
              )
            )
      prefixVar = TermParser.verifyRelativeVarName prefixDefinitionName
      dataConstructor :: P v m (Ann, (Ann, v, Type v Ann))
      dataConstructor = go <$> prefixVar <*> many TypeParser.valueTypeLeaf
      record :: P v m ([(Ann, (Ann, v, Type v Ann))], [(L.Token v, [(L.Token v, Type v Ann)])], Ann)
      record = do
        _ <- openBlockWith "{"
        let field :: P v m [(L.Token v, Type v Ann)]
            field = do
              f <- liftA2 (,) (prefixVar <* reserved ":") TypeParser.valueType
              optional (reserved ",")
                >>= ( \case
                        Nothing -> pure [f]
                        Just _ -> maybe [f] (f :) <$> (optional semi *> optional field)
                    )
        fields <- field
        closingToken <- closeBlock
        let lastSegment = name <&> (\v -> Var.named (Name.toText $ Name.unqualified (Name.unsafeParseVar v)))
        pure ([go lastSegment (snd <$> fields)], [(name, fields)], ann closingToken)
  (constructors, accessors, closingAnn) <-
    msum [Left <$> record, Right <$> sepBy (reserved "|") dataConstructor] <&> \case
      Left (constructors, accessors, closingAnn) -> (constructors, accessors, closingAnn)
      Right constructors -> do
        let closingAnn :: Ann
            closingAnn = NonEmpty.last (ann eq NonEmpty.:| ((\(constrSpanAnn, _) -> constrSpanAnn) <$> constructors))
         in (constructors, [], closingAnn)
  _ <- closeBlock
  case maybeUnresolvedModifier of
    Nothing -> do
      modifier <- resolveUniqueModifier (L.payload name)
      -- ann spanning the whole Decl.
      let declSpanAnn = ann typeToken <> closingAnn
      pure
        ( L.payload name,
          DD.mkDataDecl' modifier declSpanAnn typeArgVs (snd <$> constructors),
          accessors
        )
    Just unresolvedModifier -> do
      modifier <- resolveUnresolvedModifier unresolvedModifier (L.payload name)
      -- ann spanning the whole Decl.
      -- Technically the typeToken is redundant here, but this is more future proof.
      let declSpanAnn = ann typeToken <> ann modifier <> closingAnn
      pure
        ( L.payload name,
          DD.mkDataDecl' (L.payload modifier) declSpanAnn typeArgVs (snd <$> constructors),
          accessors
        )

effectDeclaration ::
  forall m v.
  (Monad m, Var v) =>
  Maybe (L.Token UnresolvedModifier) ->
  P v m (v, EffectDeclaration v Ann)
effectDeclaration maybeUnresolvedModifier = do
  abilityToken <- fmap void (reserved "ability") <|> openBlockWith "ability"
  name <- TermParser.verifyRelativeVarName prefixDefinitionName
  typeArgs <- many (TermParser.verifyRelativeVarName prefixDefinitionName)
  let typeArgVs = L.payload <$> typeArgs
  blockStart <- openBlockWith "where"
  constructors <- sepBy semi (constructor typeArgs name)
  -- `ability` opens a block, as does `where`
  _ <- closeBlock <* closeBlock
  let closingAnn =
        last $ ann blockStart : ((\(_, _, t) -> ann t) <$> constructors)

  case maybeUnresolvedModifier of
    Nothing -> do
      modifier <- resolveUniqueModifier (L.payload name)
      -- ann spanning the whole ability declaration.
      let abilitySpanAnn = ann abilityToken <> closingAnn
      pure
        ( L.payload name,
          DD.mkEffectDecl' modifier abilitySpanAnn typeArgVs constructors
        )
    Just unresolvedModifier -> do
      modifier <- resolveUnresolvedModifier unresolvedModifier (L.payload name)
      -- ann spanning the whole ability declaration.
      -- Technically the abilityToken is redundant here, but this is more future proof.
      let abilitySpanAnn = ann abilityToken <> ann modifier <> closingAnn
      pure
        ( L.payload name,
          DD.mkEffectDecl'
            (L.payload modifier)
            abilitySpanAnn
            typeArgVs
            constructors
        )
  where
    constructor :: [L.Token v] -> L.Token v -> P v m (Ann, v, Type v Ann)
    constructor typeArgs name =
      explodeToken
        <$> TermParser.verifyRelativeVarName prefixDefinitionName
        <* reserved ":"
        <*> ( Type.generalizeLowercase mempty
                . ensureEffect
                <$> TypeParser.computationType
            )
      where
        explodeToken v t = (ann v, Var.namespaced (L.payload name :| [L.payload v]), t)
        -- If the effect is not syntactically present in the constructor types,
        -- add them after parsing.
        ensureEffect t = case t of
          Type.Effect' _ _ -> modEffect t
          x -> Type.editFunctionResult modEffect x
        modEffect t = case t of
          Type.Effect' es t -> go es t
          t -> go [] t
        toTypeVar t = Type.av' (ann t) (Var.name $ L.payload t)
        headIs t v = case t of
          Type.Apps' (Type.Var' x) _ -> x == v
          Type.Var' x -> x == v
          _ -> False
        go es t =
          let es' =
                if any (`headIs` L.payload name) es
                  then es
                  else Type.apps' (toTypeVar name) (toTypeVar <$> typeArgs) : es
           in Type.cleanupAbilityLists $ Type.effect (ABT.annotation t) es' t
