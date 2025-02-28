module Unison.Syntax.DeclParser
  ( synDeclsP,
    SynDecl (..),
    synDeclConstructors,
    synDeclName,
    SynDataDecl (..),
    SynEffectDecl (..),
  )
where

import Control.Lens
import Data.List.NonEmpty (pattern (:|))
import Data.List.NonEmpty qualified as NonEmpty
import Unison.ABT qualified as ABT
import Unison.DataDeclaration qualified as DataDeclaration
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

data SynDecl v
  = SynDecl'Data !(SynDataDecl v)
  | SynDecl'Effect !(SynEffectDecl v)

instance Annotated (SynDecl v) where
  ann = \case
    SynDecl'Data decl -> decl.annotation
    SynDecl'Effect decl -> decl.annotation

synDeclConstructors :: SynDecl v -> [(Ann, v, Type v Ann)]
synDeclConstructors = \case
  SynDecl'Data decl -> decl.constructors
  SynDecl'Effect decl -> decl.constructors

synDeclName :: SynDecl v -> L.Token v
synDeclName = \case
  SynDecl'Data decl -> decl.name
  SynDecl'Effect decl -> decl.name

data SynDataDecl v = SynDataDecl
  { annotation :: !Ann,
    constructors :: ![(Ann, v, Type v Ann)],
    fields :: !(Maybe [(L.Token v, Type v Ann)]),
    modifier :: !DataDeclaration.Modifier,
    name :: !(L.Token v),
    tyvars :: ![v]
  }
  deriving stock (Generic)

data SynEffectDecl v = SynEffectDecl
  { annotation :: !Ann,
    constructors :: ![(Ann, v, Type v Ann)],
    modifier :: !DataDeclaration.Modifier,
    name :: !(L.Token v),
    tyvars :: ![v]
  }
  deriving stock (Generic)

synDeclsP :: (Monad m, Var v) => P v m [SynDecl v]
synDeclsP =
  many (synDeclP <* optional semi)

-- | When we first walk over the modifier, it may be a `unique`, in which case we want to use a function in the parsing
-- environment to map the type's name (which we haven't parsed yet) to a GUID to reuse (if any).
--
-- So, we parse into this temporary "unresolved modifier" type, which is soon resolved to a real Modifier once we know
-- the type name.
data UnresolvedModifier
  = UnresolvedModifier'Structural
  | UnresolvedModifier'UniqueWithGuid !Text
  | UnresolvedModifier'UniqueWithoutGuid

-- unique[someguid] type Blah = ...
modifierP :: (Monad m, Var v) => P v m (Maybe (L.Token UnresolvedModifier))
modifierP = do
  optional (unique <|> structural)
  where
    unique = do
      tok <- openBlockWith "unique"
      optional (openBlockWith "[" *> importRelativeWordyId <* closeBlock) >>= \case
        Nothing -> pure (UnresolvedModifier'UniqueWithoutGuid <$ tok)
        Just guid -> pure (UnresolvedModifier'UniqueWithGuid (Name.toText (L.payload guid)) <$ tok)
    structural = do
      tok <- openBlockWith "structural"
      pure (UnresolvedModifier'Structural <$ tok)

synDeclP :: (Monad m, Var v) => P v m (SynDecl v)
synDeclP = do
  modifier <- modifierP
  SynDecl'Effect <$> synEffectDeclP modifier <|> SynDecl'Data <$> synDataDeclP modifier

synDataDeclP :: forall m v. (Monad m, Var v) => Maybe (L.Token UnresolvedModifier) -> P v m (SynDataDecl v)
synDataDeclP modifier0 = do
  typeToken <- fmap void (reserved "type") <|> openBlockWith "type"
  (name, typeArgs) <- (,) <$> prefixVar <*> many prefixVar
  let tyvars = L.payload <$> typeArgs
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
            ctorAnn = ann ctorName <> maybe mempty ann (lastMay ctorArgs)
         in ( ctorAnn,
              ( ann ctorName,
                Var.namespaced (L.payload name :| [L.payload ctorName]),
                Type.foralls ctorAnn tyvars ctorType
              )
            )
      record :: P v m ((Ann, v, Type v Ann), Maybe [(L.Token v, Type v Ann)], Ann)
      record = do
        _ <- openBlockWith "{"
        let field :: P v m [(L.Token v, Type v Ann)]
            field = do
              f <- liftA2 (,) (prefixVar <* reserved ":") TypeParser.valueType
              optional (reserved ",") >>= \case
                Nothing -> pure [f]
                Just _ -> maybe [f] (f :) <$> (optional semi *> optional field)
        fields <- field
        closingToken <- closeBlock
        let lastSegment = name <&> (\v -> Var.named (Name.toText $ Name.unqualified (Name.unsafeParseVar v)))
        pure (snd (go lastSegment (snd <$> fields)), Just fields, ann closingToken)
  optional record >>= \case
    Nothing -> do
      constructors <- sepBy (reserved "|") (go <$> prefixVar <*> many TypeParser.valueTypeLeaf)
      _ <- closeBlock
      let closingAnn :: Ann
          closingAnn = NonEmpty.last (ann eq NonEmpty.:| ((\(constrSpanAnn, _) -> constrSpanAnn) <$> constructors))
      modifier <- resolveModifier name modifier0
      pure
        SynDataDecl
          { annotation = maybe (ann typeToken) ann modifier0 <> closingAnn,
            constructors = snd <$> constructors,
            fields = Nothing,
            modifier,
            name,
            tyvars
          }
    Just (constructor, fields, closingAnn) -> do
      _ <- closeBlock
      modifier <- resolveModifier name modifier0
      pure
        SynDataDecl
          { annotation = maybe (ann typeToken) ann modifier0 <> closingAnn,
            constructors = [constructor],
            fields,
            modifier,
            name,
            tyvars
          }
  where
    prefixVar :: P v m (L.Token v)
    prefixVar =
      TermParser.verifyRelativeVarName prefixDefinitionName

synEffectDeclP :: forall m v. (Monad m, Var v) => Maybe (L.Token UnresolvedModifier) -> P v m (SynEffectDecl v)
synEffectDeclP modifier0 = do
  abilityToken <- fmap void (reserved "ability") <|> openBlockWith "ability"
  name <- TermParser.verifyRelativeVarName prefixDefinitionName
  typeArgs <- many (TermParser.verifyRelativeVarName prefixDefinitionName)
  blockStart <- openBlockWith "where"
  constructors <- sepBy semi (effectConstructorP typeArgs name)
  -- `ability` opens a block, as does `where`
  _ <- closeBlock <* closeBlock
  let closingAnn =
        last $ ann blockStart : ((\(_, _, t) -> ann t) <$> constructors)
  modifier <- resolveModifier name modifier0
  pure
    SynEffectDecl
      { annotation = maybe (ann abilityToken) ann modifier0 <> closingAnn,
        constructors,
        modifier,
        name,
        tyvars = L.payload <$> typeArgs
      }

effectConstructorP :: (Monad m, Var v) => [L.Token v] -> L.Token v -> P v m (Ann, v, Type v Ann)
effectConstructorP typeArgs name =
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

resolveModifier :: (Monad m, Var v) => L.Token v -> Maybe (L.Token UnresolvedModifier) -> P v m DataDeclaration.Modifier
resolveModifier name modifier =
  case L.payload <$> modifier of
    Just UnresolvedModifier'Structural -> pure DataDeclaration.Structural
    Just (UnresolvedModifier'UniqueWithGuid guid) -> pure (DataDeclaration.Unique guid)
    Just UnresolvedModifier'UniqueWithoutGuid -> resolveUniqueTypeGuid name.payload
    Nothing -> resolveUniqueTypeGuid name.payload
