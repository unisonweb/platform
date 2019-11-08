{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Unison.PrintError where

import Unison.Prelude

import           Control.Lens                 ((%~))
import           Control.Lens.Tuple           (_1, _2, _3)
import           Data.List                    (intersperse)
import           Data.List.Extra              (nubOrd)
import qualified Data.List.NonEmpty           as Nel
import qualified Data.Map                     as Map
import           Data.Sequence                (Seq (..))
import qualified Data.Set                     as Set
import qualified Data.Text                    as Text
import           Data.Void                    (Void)
import qualified Text.Megaparsec              as P
import qualified Unison.ABT                   as ABT
import Unison.DataDeclaration (pattern TupleType')
import qualified Unison.HashQualified         as HQ
import           Unison.Kind                  (Kind)
import qualified Unison.Kind                  as Kind
import qualified Unison.Lexer                 as L
import           Unison.Parser                (Ann (..), Annotated, ann)
import qualified Unison.Parser                as Parser
import qualified Unison.Reference             as R
import           Unison.Referent              (Referent)
import           Unison.Result                (Note (..))
import qualified Unison.Settings              as Settings
import qualified Unison.Term                  as Term
import qualified Unison.Type                  as Type
import qualified Unison.Typechecker.Context   as C
import           Unison.Typechecker.TypeError
import qualified Unison.TypeVar               as TypeVar
import qualified Unison.UnisonFile            as UF
import           Unison.Util.AnnotatedText    (AnnotatedText)
import qualified Unison.Util.AnnotatedText    as AT
import           Unison.Util.ColorText        (Color)
import qualified Unison.Util.ColorText        as Color
import           Unison.Util.Monoid           (intercalateMap)
import           Unison.Util.Range            (Range (..), startingLine)
import           Unison.Var                   (Var)
import qualified Unison.Var                   as Var
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.TermPrinter as TermPrinter
import qualified Unison.Util.Pretty as Pr
import Unison.Util.Pretty (Pretty, ColorText)
import qualified Unison.Names3 as Names
import qualified Unison.Name as Name
import Unison.HashQualified (HashQualified)
import Unison.Type (Type)
import Unison.NamePrinter (prettyHashQualified0)

type Env = PPE.PrettyPrintEnv

pattern Code = Color.Blue
pattern Type1 = Color.HiBlue
pattern Type2 = Color.Green
pattern ErrorSite = Color.HiRed
pattern TypeKeyword = Color.Yellow
pattern AbilityKeyword = Color.Green
pattern Identifier = Color.Bold

defaultWidth :: Pr.Width
defaultWidth = 60

fromOverHere'
  :: Ord a
  => String
  -> [Maybe (Range, a)]
  -> [Maybe (Range, a)]
  -> Pretty (AnnotatedText a)
fromOverHere' s spots0 removing =
  fromOverHere s (catMaybes spots0) (catMaybes removing)

fromOverHere
  :: Ord a => String -> [(Range, a)] -> [(Range, a)] -> Pretty (AnnotatedText a)
fromOverHere src spots0 removing =
  let spots = toList $ Set.fromList spots0 Set.\\ Set.fromList removing
  in  case length spots of
        0 -> mempty
        1 -> "\n  from right here:\n\n" <> showSource src spots
        _ -> "\n  from these spots, respectively:\n\n" <> showSource src spots

showTypeWithProvenance
  :: (Var v, Annotated a, Ord style)
  => Env
  -> String
  -> style
  -> Type v a
  -> Pretty (AnnotatedText style)
showTypeWithProvenance env src color typ =
  style color (renderType' env typ)
    <> ".\n"
    <> fromOverHere' src [styleAnnotated color typ] []

styleAnnotated :: Annotated a => sty -> a -> Maybe (Range, sty)
styleAnnotated sty a = (, sty) <$> rangeForAnnotated a

style :: s -> String -> Pretty (AnnotatedText s)
style sty str = Pr.lit . AT.annotate sty $ fromString str

stylePretty :: Color -> Pretty ColorText -> Pretty ColorText
stylePretty sty str = Pr.map (AT.annotate sty) str

describeStyle :: Color -> Pretty ColorText
describeStyle ErrorSite = "in " <> style ErrorSite "red"
describeStyle Type1     = "in " <> style Type1 "blue"
describeStyle Type2     = "in " <> style Type2 "green"
describeStyle _         = ""

-- Render an informational typechecking note
renderTypeInfo
  :: forall v loc sty
   . (Var v, Annotated loc, Ord loc, Show loc)
  => TypeInfo v loc
  -> Env
  -> Pretty (AnnotatedText sty)
renderTypeInfo i env = case i of
  TopLevelComponent {..} -> case definitions of
    [def] ->
      Pr.wrap "🌟 I found and typechecked a definition:" <> Pr.newline <> mconcat
        (renderOne def)
    [] -> mempty
    _ ->
      Pr.wrap "🎁 These mutually dependent definitions typechecked:"
        <> Pr.newline
        <> intercalateMap Pr.newline (foldMap ("\t" <>) . renderOne) definitions
 where
  renderOne :: IsString s => (v, Type v loc, RedundantTypeAnnotation) -> [s]
  renderOne (v, typ, _) =
    [fromString . Text.unpack $ Var.name v, " : ", renderType' env typ]


-- Render a type error
renderTypeError
  :: forall v loc
   . (Var v, Annotated loc, Ord loc, Show loc)
  => TypeError v loc
  -> Env
  -> String
  -> Pretty ColorText
renderTypeError e env src = case e of
  BooleanMismatch {..} -> mconcat
    [ Pr.wrap $ mconcat
        [ preamble
        , " "
        , style Type1 "Boolean"
        , ", but this one is "
        , style Type2 (renderType' env foundType)
        , ":"
        ]
    , Pr.lineSkip
    , showSourceMaybes src [siteS]
    , fromOverHere' src [typeS] [siteS]
    , debugNoteLoc $ mconcat
      [ "loc debug:"
      , "\n  mismatchSite: "
      , annotatedToEnglish mismatchSite
      , "\n     foundType: "
      , annotatedToEnglish foundType
      , "\n"
      ]
    , debugSummary note
    ]
   where
    siteS    = styleAnnotated Type2 mismatchSite
    typeS    = styleAnnotated Type2 foundType
    preamble = case getBooleanMismatch of
      CondMismatch ->
        "The condition for an "
          <> style ErrorSite "if"
          <> "-expression has to be"
      AndMismatch ->
        "The arguments to " <> style ErrorSite "and" <> " have to be"
      OrMismatch ->
        "The arguments to " <> style ErrorSite "or" <> " have to be"
      GuardMismatch ->
        "The guard expression for a "
          <> style ErrorSite "case"
          <> " has to be"

  ExistentialMismatch {..} -> mconcat
    [ Pr.wrap $ mconcat
        [ preamble
        , " "
        , "Here, one is "
        , style Type1 (renderType' env expectedType)
        , " and another is "
        , style Type2 (renderType' env foundType)
        , ":"]
    , Pr.lineSkip
    , showSourceMaybes src [mismatchSiteS, expectedLocS]
    , fromOverHere' src
                    [expectedTypeS, mismatchedTypeS]
                    [mismatchSiteS, expectedLocS]
    , intLiteralSyntaxTip mismatchSite expectedType
    , debugNoteLoc $ mconcat
      [ "\nloc debug:"
      , "\n  mismatchSite: "
      , annotatedToEnglish mismatchSite
      , "\n     foundType: "
      , annotatedToEnglish foundType
      , "\n  expectedType: "
      , annotatedToEnglish expectedType
      , "\n   expectedLoc: "
      , annotatedToEnglish expectedLoc
      , "\n"
      ]
    , debugSummary note
    ]
   where
    mismatchedTypeS = styleAnnotated Type2 foundType
    mismatchSiteS   = styleAnnotated Type2 mismatchSite
    expectedTypeS   = styleAnnotated Type1 expectedType
    expectedLocS    = styleAnnotated Type1 expectedLoc
    preamble        = case getExistentialMismatch of
      IfBody -> mconcat
        [ "The "
        , style ErrorSite "else"
        , " clause of an "
        , style ErrorSite "if"
        , " expression needs to have the same type as the "
        , style ErrorSite "then"
        , " clause."
        ]
      VectorBody -> "The elements of a vector all need to have the same type."
      CaseBody   -> mconcat
        [ "Each case of a "
        , style ErrorSite "case"
        , "/"
        , style ErrorSite "of"
        , " expression "
        , "need to have the same type."
        ]
  NotFunctionApplication {..} -> mconcat
    [ "This looks like a function call, but with a "
    , style Type1 (renderType' env ft)
    , " where the function should be.  Are you missing an operator?\n\n"
    , annotatedAsStyle Type1 src f
    , debugSummary note
    ]
  FunctionApplication {..}
    -> let
         fte         = Type.removePureEffects ft
         fteFreeVars = Set.map TypeVar.underlying $ ABT.freeVars fte
         showVar (v, _t) = Set.member v fteFreeVars
         solvedVars' = filter showVar solvedVars
       in
         mconcat
           [ "The "
           , ordinal argNum
           , " argument to the function "
           , style ErrorSite (renderTerm env f)
           , " is "
           , style Type2 (renderType' env foundType)
           , ", but I was expecting "
           , style Type1 (renderType' env expectedType)
           , ":\n\n"
           , showSourceMaybes src
             [ (, Type1) <$> rangeForAnnotated expectedType
             , (, Type2) <$> rangeForAnnotated foundType
             , (, Type2) <$> rangeForAnnotated arg
             , (, ErrorSite) <$> rangeForAnnotated f ]
           , intLiteralSyntaxTip arg expectedType
         -- todo: factor this out and use in ExistentialMismatch and any other
         --       "recursive subtypes" situations
           , case leafs of
             Nothing                        -> mempty
             Just (foundLeaf, expectedLeaf) -> mconcat
               [ "\n"
               , "More specifically, I found "
               , style Type2 (renderType' env foundLeaf)
               , " where I was expecting "
               , style Type1 (renderType' env expectedLeaf)
               , ":\n\n"
               , showSourceMaybes
                 src
                 [ (, Type1) <$> rangeForAnnotated expectedLeaf
                 , (, Type2) <$> rangeForAnnotated foundLeaf
                 ]
               ]
           , case solvedVars' of
             _ : _ ->
               let
                 go :: (v, C.Type v loc) -> Pretty ColorText
                 go (v, t) = mconcat
                   [ " "
                   , renderVar v
                   , " = "
                   , style ErrorSite (renderType' env t)
                   , ", from here:\n\n"
                   , showSourceMaybes
                     src
                     [(, ErrorSite) <$> rangeForAnnotated t]
                   , "\n"
                   ]
               in
                 mconcat
                   [ "\n"
                   , "because the "
                   , style ErrorSite (renderTerm env f)
                   , " function has type"
                   , "\n\n"
                   , "  "
                   , renderType' env fte
                   , "\n\n"
                   , "where:"
                   , "\n\n"
                   , mconcat (go <$> solvedVars')
                   ]
             [] -> mempty
           , debugNoteLoc
           . mconcat
           $ [ "\nloc debug:"
             , style ErrorSite "\n             f: "
             , annotatedToEnglish f
             , style Type2 "\n     foundType: "
             , annotatedToEnglish foundType
             , style Type1 "\n  expectedType: "
             , annotatedToEnglish expectedType
             -- , "\n   expectedLoc: ", annotatedToEnglish expectedLoc
             ]
           , debugSummary note
           ]
  Mismatch {..} -> mconcat
    [ "I found a value of type "
    , style Type1 (renderType' env foundLeaf)
    , " where I expected to find one of type "
    , style Type2 (renderType' env expectedLeaf)
    , ":\n\n"
    , showSourceMaybes
      src
      [ -- these are overwriting the colored ranges for some reason?
        --   (,Color.ForceShow) <$> rangeForAnnotated mismatchSite
        -- , (,Color.ForceShow) <$> rangeForType foundType
        -- , (,Color.ForceShow) <$> rangeForType expectedType
        -- ,
        (, Type1) <$> rangeForAnnotated mismatchSite
      , (, Type2) <$> rangeForAnnotated expectedLeaf
      ]
    , fromOverHere' src
                    [styleAnnotated Type1 foundLeaf]
                    [styleAnnotated Type1 mismatchSite]
    , intLiteralSyntaxTip mismatchSite expectedType
    , debugNoteLoc
    . mconcat
    $ [ "\nloc debug:"
      , "\n  mismatchSite: "
      , annotatedToEnglish mismatchSite
      , "\n     foundType: "
      , annotatedToEnglish foundType
      , "\n     foundLeaf: "
      , annotatedToEnglish foundLeaf
      , "\n  expectedType: "
      , annotatedToEnglish expectedType
      , "\n  expectedLeaf: "
      , annotatedToEnglish expectedLeaf
      , "\n"
      ]
    , debugSummary note
    ]
  AbilityCheckFailure {..} -> mconcat
    [ "The expression "
    , describeStyle ErrorSite
    , " "
    , case toList requested of
      []  -> error "unpossible"
      [e] -> "needs the {" <> renderType' env e <> "} ability,"
      requested ->
        " needs these abilities: {"
          <> commas (renderType' env) requested
          <> "},"
    , " but "
    , case toList ambient of
      [] -> "this location does not have access to any abilities."
      [e] ->
        "this location only has access to the {"
          <> renderType' env e
          <> "} ability,"
      ambient ->
        "this location only has access to these abilities: "
          <> "{"
          <> commas (renderType' env) ambient
          <> "}"
    , "\n\n"
    , annotatedAsErrorSite src abilityCheckFailureSite
    , debugSummary note
    ]
  UnguardedLetRecCycle vs locs _ -> mconcat
    [ "These definitions depend on each other cyclically but aren't guarded "
    , "by a lambda: " <> intercalateMap ", " renderVar vs
    , "\n"
    , showSourceMaybes src [ (,ErrorSite) <$> rangeForAnnotated loc | loc <- locs ]]

  UnknownType {..} -> mconcat [
    if ann typeSite == Intrinsic then
      "I don't know about the builtin type " <> style ErrorSite (renderVar unknownTypeV) <> ". "
    else if ann typeSite == External then
      "I don't know about the type " <> style ErrorSite (renderVar unknownTypeV) <> ". "
    else
      "I don't know about the type " <> style ErrorSite (renderVar unknownTypeV) <> ":\n"
      <> annotatedAsErrorSite src typeSite
    , "Make sure it's imported and spelled correctly."
    ]
  UnknownTerm {..} ->
    let (correct, wrongTypes, wrongNames) =
          foldr sep id suggestions ([], [], [])
        sep (C.Suggestion name typ _ match) r =
          case match of
            C.Exact -> (_1 %~ ((name, typ) :)) . r
            C.WrongType -> (_2 %~ ((name, typ) :)) . r
            C.WrongName -> (_3 %~ ((name, typ) :)) . r
    in  mconcat
          [ "I'm not sure what "
          , style ErrorSite (Var.nameStr unknownTermV)
          , " means at "
          , annotatedToEnglish termSite
          , "\n\n"
          , annotatedAsErrorSite src termSite
          , case expectedType of
            Type.Existential' _ _ -> "\nThere are no constraints on its type."
            _ ->
              "\nWhatever it is, it has a type that conforms to "
                <> style Type1 (renderType' env expectedType)
                <> ".\n"
                 -- ++ showTypeWithProvenance env src Type1 expectedType
          , case correct of
            [] -> case wrongTypes of
              [] -> case wrongNames of
                []     -> mempty
                wrongs -> formatWrongs wrongNameText wrongs
              wrongs -> formatWrongs wrongTypeText wrongs
            suggs -> mconcat
              [ "I found some terms in scope that have matching names and types. "
              , "Maybe you meant one of these:\n\n"
              , intercalateMap "\n" formatSuggestion suggs
              ]
          ]
  Other (C.cause -> C.HandlerOfUnexpectedType loc typ) ->   
    Pr.lines [
      Pr.wrap "The handler used here", "",
      annotatedAsErrorSite src loc, 
      Pr.wrap $
        "has type " <> stylePretty ErrorSite (Pr.group (renderType' env typ))
        <> "but I'm expecting a function of the form" 
        <> Pr.group (Pr.blue (renderType' env exHandler) <> ".")
     ] 
    where
    exHandler :: C.Type v loc
    exHandler = fmap (const loc) $
      Type.arrow () 
        (Type.apps' (Type.ref () Type.effectRef) 
           [Type.var () (Var.named "e"), Type.var () (Var.named "a") ])
        (Type.var () (Var.named "o"))

  Other note -> mconcat
    [ "Sorry, you hit an error we didn't make a nice message for yet.\n\n"
    , "Here is a summary of the Note:\n"
    , summary note
    ]
 where
  wrongTypeText pl = mconcat
    [ "I found "
    , pl "a term" "some terms"
    , " in scope with "
    , pl "a " ""
    , "matching name"
    , pl "" "s"
    , " but "
    , pl "a " ""
    , "different type"
    , pl "" "s"
    , ". "
    , "If "
    , pl "this" "one of these"
    , " is what you meant, try using the fully qualified name and I might "
    , "be able to give you a more illuminating error message: \n\n"
    ]
  wrongNameText pl = mconcat
    [ "I found "
    , pl "a term" "some terms"
    , " in scope with "
    , pl "a " ""
    , "matching type"
    , pl "" "s"
    , " but "
    , pl "a " ""
    , "different name"
    , pl "" "s"
    , ". "
    , "Maybe you meant "
    , pl "this" "one of these"
    , ":\n\n"
    ]
  formatSuggestion :: (Text, C.Type v loc) -> Pretty ColorText
  formatSuggestion (name, typ) =
    "  - " <> fromString (Text.unpack name) <> " : " <> renderType' env typ
  formatWrongs txt wrongs =
    let sz = length wrongs
        pl a b = if sz == 1 then a else b
    in  mconcat [txt pl, intercalateMap "\n" formatSuggestion wrongs]
  ordinal :: (IsString s) => Int -> s
  ordinal n = fromString $ show n ++ case last (show n) of
    '1' -> "st"
    '2' -> "nd"
    '3' -> "rd"
    _   -> "th"
  debugNoteLoc a = if Settings.debugNoteLoc then a else mempty
  debugSummary :: C.ErrorNote v loc -> Pretty ColorText
  debugSummary note =
    if Settings.debugNoteSummary then summary note else mempty
  summary :: C.ErrorNote v loc -> Pretty ColorText
  summary note = mconcat
    [ "\n"
    , "  simple cause:\n"
    , "    "
    , simpleCause (C.cause note)
    , "\n"
    , case toList (C.path note) of
      [] -> "  path: (empty)\n"
      l  -> "  path:\n" <> mconcat (simplePath <$> l)
    ]
  simplePath :: C.PathElement v loc -> Pretty ColorText
  simplePath e = "    " <> simplePath' e <> "\n"
  simplePath' :: C.PathElement v loc -> Pretty ColorText
  simplePath' = \case
    C.InSynthesize e -> "InSynthesize e=" <> renderTerm env e
    C.InSubtype t1 t2 ->
      "InSubtype t1=" <> renderType' env t1 <> ", t2=" <> renderType' env t2
    C.InCheck e t ->
      "InCheck e=" <> renderTerm env e <> "," <> " t=" <> renderType' env t
    C.InInstantiateL v t ->
      "InInstantiateL v=" <> renderVar v <> ", t=" <> renderType' env t
    C.InInstantiateR t v ->
      "InInstantiateR t=" <> renderType' env t <> " v=" <> renderVar v
    C.InSynthesizeApp t e n ->
      "InSynthesizeApp t="
        <> renderType' env t
        <> ", e="
        <> renderTerm env e
        <> ", n="
        <> fromString (show n)
    C.InFunctionCall vs f ft es ->
      "InFunctionCall vs=["
        <> commas renderVar vs
        <> "]"
        <> ", f="
        <> renderTerm env f
        <> ", ft="
        <> renderType' env ft
        <> ", es=["
        <> commas (renderTerm env) es
        <> "]"
    C.InIfCond        -> "InIfCond"
    C.InIfBody loc    -> "InIfBody thenBody=" <> annotatedToEnglish loc
    C.InAndApp        -> "InAndApp"
    C.InOrApp         -> "InOrApp"
    C.InVectorApp loc -> "InVectorApp firstTerm=" <> annotatedToEnglish loc
    C.InMatch     loc -> "InMatch firstBody=" <> annotatedToEnglish loc
    C.InMatchGuard    -> "InMatchGuard"
    C.InMatchBody     -> "InMatchBody"
  simpleCause :: C.Cause v loc -> Pretty ColorText
  simpleCause = \case
    C.TypeMismatch c ->
      mconcat ["TypeMismatch\n", "  context:\n", renderContext env c]
    C.HandlerOfUnexpectedType loc typ ->
      mconcat ["HandlerOfUnexpectedType\n", Pr.shown loc, "type:\n", renderType' env typ ]
    C.IllFormedType c ->
      mconcat ["IllFormedType\n", "  context:\n", renderContext env c]
    C.UnguardedLetRecCycle vs _ts ->
      "Unguarded cycle of definitions: " <>
      foldMap renderVar vs
    C.UnknownSymbol loc v -> mconcat
      [ "UnknownSymbol: "
      , annotatedToEnglish loc
      , " " <> renderVar v
      , "\n\n"
      , annotatedAsErrorSite src loc
      ]
    C.UnknownTerm loc v suggestions typ -> mconcat
      [ "UnknownTerm: "
      , annotatedToEnglish loc
      , " "
      , renderVar v
      , "\n\n"
      , annotatedAsErrorSite src loc
      , "Suggestions: "
      , mconcat (renderSuggestion env <$> suggestions)
      , "\n\n"
      , "Type: "
      , renderType' env typ
      ]
    C.CompilerBug c -> "CompilerBug: " <> fromString (show c)
    C.AbilityCheckFailure ambient requested c -> mconcat
      [ "AbilityCheckFailure: "
      , "ambient={"
      , commas (renderType' env) ambient
      , "} requested={"
      , commas (renderType' env) requested
      , "}\n"
      , renderContext env c
      ]
    C.EffectConstructorWrongArgCount e a r cid -> mconcat
      [ "EffectConstructorWrongArgCount:"
      , "  expected="
      , (fromString . show) e
      , ", actual="
      , (fromString . show) a
      , ", reference="
      , showConstructor env r cid
      ]
    C.MalformedEffectBind ctorType ctorResult es -> mconcat
      [ "MalformedEffectBind: "
      , "  ctorType="
      , renderType' env ctorType
      , "  ctorResult="
      , renderType' env ctorResult
      , "  effects="
      , fromString (show es)
      ]
    C.PatternArityMismatch loc typ args -> mconcat
      [ "PatternArityMismatch:\n"
      , "  loc="
      , annotatedToEnglish loc
      , "\n"
      , "  typ="
      , renderType' env typ
      , "\n"
      , "  args="
      , fromString (show args)
      , "\n"
      ]
    C.DuplicateDefinitions vs ->
      let go :: (v, [loc]) -> Pretty (AnnotatedText a)
          go (v, locs) =
            "["
              <> renderVar v
              <> mconcat (intersperse " : " $ annotatedToEnglish <$> locs)
              <> "]"
      in  "DuplicateDefinitions:" <> mconcat (go <$> Nel.toList vs)
    C.ConcatPatternWithoutConstantLength loc typ -> mconcat
      [ "ConcatPatternWithoutConstantLength:\n"
      , "  loc="
      , annotatedToEnglish loc
      , "\n"
      , "  typ="
      , renderType' env typ
      , "\n"
      ]

renderContext
  :: (Var v, Ord loc) => Env -> C.Context v loc -> Pretty (AnnotatedText a)
renderContext env ctx@(C.Context es) = "  Γ\n    "
  <> intercalateMap "\n    " (showElem ctx . fst) (reverse es)
 where
  shortName :: (Var v, IsString loc) => v -> loc
  shortName = fromString . Text.unpack . Var.name
  showElem
    :: (Var v, Ord loc)
    => C.Context v loc
    -> C.Element v loc
    -> Pretty (AnnotatedText a)
  showElem _ctx (C.Var v) = case v of
    TypeVar.Universal x     -> "@" <> renderVar x
    TypeVar.Existential _ x -> "'" <> renderVar x
  showElem ctx (C.Solved _ v (Type.Monotype t)) =
    "'" <> shortName v <> " = " <> renderType' env (C.apply ctx t)
  showElem ctx (C.Ann v t) =
    shortName v <> " : " <> renderType' env (C.apply ctx t)
  showElem _ (C.Marker v) = "|" <> shortName v <> "|"

renderTerm :: (IsString s, Var v) => Env -> C.Term v loc -> s
renderTerm env e =
  let s = Color.toPlain $ TermPrinter.pretty' (Just 80) env (Term.unTypeVar e)
  in if length s > Settings.renderTermMaxLength
     then fromString (take Settings.renderTermMaxLength s <> "...")
     else fromString s

-- | renders a type with no special styling
renderType' :: (IsString s, Var v) => Env -> Type v loc -> s
renderType' env typ =
  fromString . Pr.toPlain defaultWidth $ renderType env (const id) typ

-- | `f` may do some styling based on `loc`.
-- | You can pass `(const id)` if no styling is needed, or call `renderType'`.
renderType
  :: Var v
  => Env
  -> (loc -> Pretty (AnnotatedText a) -> Pretty (AnnotatedText a))
  -> Type v loc
  -> Pretty (AnnotatedText a)
renderType env f t = renderType0 env f (0 :: Int) (Type.removePureEffects t)
 where
  wrap :: (IsString a, Semigroup a) => a -> a -> Bool -> a -> a
  wrap start end test s = if test then start <> s <> end else s
  paren = wrap "(" ")"
  curly = wrap "{" "}"
  renderType0 env f p t = f (ABT.annotation t) $ case t of
    Type.Ref' r -> showTypeRef env r
    Type.Arrow' i (Type.Effect1' e o) ->
      paren (p >= 2) $ go 2 i <> " ->{" <> go 1 e <> "} " <> go 1 o
    Type.Arrow' i o -> paren (p >= 2) $ go 2 i <> " -> " <> go 1 o
    Type.Ann'   t k -> paren True $ go 1 t <> " : " <> renderKind k
    TupleType' ts   -> paren True $ commas (go 0) ts
    Type.Apps' (Type.Ref' (R.Builtin "Sequence")) [arg] ->
      "[" <> go 0 arg <> "]"
    Type.Apps' f' args -> paren (p >= 3) $ spaces (go 3) (f' : args)
    Type.Effects' es   -> curly (p >= 3) $ commas (go 0) es
    Type.Effect' es t  -> case es of
      [] -> go p t
      _  -> "{" <> commas (go 0) es <> "} " <> go 3 t
    Type.Effect1' e t -> paren (p >= 3) $ "{" <> go 0 e <> "}" <> go 3 t
    Type.ForallsNamed' vs body ->
      paren (p >= 1) $ if not Settings.debugRevealForalls
        then go 0 body
        else "forall " <> spaces renderVar vs <> " . " <> go 1 body
    Type.Var' v -> renderVar v
    _ -> error $ "pattern match failure in PrintError.renderType " ++ show t
    where go = renderType0 env f

renderSuggestion
  :: (IsString s, Semigroup s, Var v) => Env -> C.Suggestion v loc -> s
renderSuggestion env sug =
  fromString (Text.unpack $ C.suggestionName sug) <> " : " <> renderType'
    env
    (C.suggestionType sug)

spaces :: (IsString a, Monoid a) => (b -> a) -> [b] -> a
spaces = intercalateMap " "

arrows :: (IsString a, Monoid a) => (b -> a) -> [b] -> a
arrows = intercalateMap " ->"

commas :: (IsString a, Monoid a) => (b -> a) -> [b] -> a
commas = intercalateMap ", "

renderVar :: (IsString a, Var v) => v -> a
renderVar = fromString . Text.unpack . Var.name

renderVar' :: (Var v, Annotated a) => Env -> C.Context v a -> v -> String
renderVar' env ctx v = case C.lookupSolved ctx v of
  Nothing -> "unsolved"
  Just t  -> renderType' env $ Type.getPolytype t

prettyVar :: Var v => v -> Pretty ColorText
prettyVar = Pr.text . Var.name

renderKind :: Kind -> Pretty (AnnotatedText a)
renderKind Kind.Star          = "*"
renderKind (Kind.Arrow k1 k2) = renderKind k1 <> " -> " <> renderKind k2

showTermRef :: IsString s => Env -> Referent -> s
showTermRef env r = fromString . HQ.toString $ PPE.termName env r

showTypeRef :: IsString s => Env -> R.Reference -> s
showTypeRef env r = fromString . HQ.toString $ PPE.typeName env r

-- todo: do something different/better if cid not found
showConstructor :: IsString s => Env -> R.Reference -> Int -> s
showConstructor env r cid = fromString . HQ.toString $
  PPE.patternName env r cid

styleInOverallType
  :: (Var v, Annotated a, Eq a)
  => Env
  -> C.Type v a
  -> C.Type v a
  -> Color
  -> Pretty ColorText
styleInOverallType e overallType leafType c = renderType e f overallType
 where
  f loc s = if loc == ABT.annotation leafType then Color.style c <$> s else s

_posToEnglish :: IsString s => L.Pos -> s
_posToEnglish (L.Pos l c) =
  fromString $ "Line " ++ show l ++ ", Column " ++ show c

rangeForToken :: L.Token a -> Range
rangeForToken t = Range (L.start t) (L.end t)

rangeToEnglish :: IsString s => Range -> s
rangeToEnglish (Range (L.Pos l c) (L.Pos l' c')) =
  fromString
    $ let showColumn = True
      in
        if showColumn
          then if l == l'
            then if c == c'
              then "line " ++ show l ++ ", column " ++ show c
              else "line " ++ show l ++ ", columns " ++ show c ++ "-" ++ show c'
            else
              "line "
              ++ show l
              ++ ", column "
              ++ show c
              ++ " through "
              ++ "line "
              ++ show l'
              ++ ", column "
              ++ show c'
          else if l == l'
            then "line " ++ show l
            else "lines " ++ show l ++ "—" ++ show l'

annotatedToEnglish :: (Annotated a, IsString s) => a -> s
annotatedToEnglish a = case ann a of
  Intrinsic     -> "an intrinsic"
  External      -> "an external"
  Ann start end -> rangeToEnglish $ Range start end


rangeForAnnotated :: Annotated a => a -> Maybe Range
rangeForAnnotated a = case ann a of
  Intrinsic     -> Nothing
  External      -> Nothing
  Ann start end -> Just $ Range start end

showLexerOutput :: Bool
showLexerOutput = False

renderNoteAsANSI
  :: (Var v, Annotated a, Show a, Ord a)
  => Pr.Width
  -> Env
  -> String
  -> Note v a
  -> String
renderNoteAsANSI w e s n = Pr.toANSI w $ printNoteWithSource e s n

renderParseErrorAsANSI :: Var v => Pr.Width -> String -> Parser.Err v -> String
renderParseErrorAsANSI w src = Pr.toANSI w . prettyParseError src

printNoteWithSource
  :: (Var v, Annotated a, Show a, Ord a)
  => Env
  -> String
  -> Note v a
  -> Pretty ColorText
printNoteWithSource env  _s (TypeInfo  n) = prettyTypeInfo n env
printNoteWithSource _env s  (Parsing   e) = prettyParseError s e
printNoteWithSource env  s  (TypeError e) = prettyTypecheckError e env s
printNoteWithSource _env _s   (NameResolutionFailures _es) = undefined
printNoteWithSource _env s (InvalidPath path term) =
  fromString ("Invalid Path: " ++ show path ++ "\n")
    <> annotatedAsErrorSite s term
printNoteWithSource _env s (UnknownSymbol v a) =
  fromString ("Unknown symbol `" ++ Text.unpack (Var.name v) ++ "`\n\n")
    <> annotatedAsErrorSite s a
printNoteWithSource _env _s (CompilerBug c) =
  fromString $ "Compiler bug: " <> show c

_printPosRange :: String -> L.Pos -> L.Pos -> String
_printPosRange s (L.Pos startLine startCol) _end =
  -- todo: multi-line ranges
  -- todo: ranges
  _printArrowsAtPos s startLine startCol

_printArrowsAtPos :: String -> Int -> Int -> String
_printArrowsAtPos s line column =
  let lineCaret s i = s ++ if i == line then "\n" ++ columnCaret else ""
      columnCaret = replicate (column - 1) '-' ++ "^"
      source      = unlines (uncurry lineCaret <$> lines s `zip` [1 ..])
  in  source

-- Wow, epic view pattern for picking out a lexer error
pattern LexerError ts e <- Just (P.Tokens (firstLexerError -> Just (ts, e)))

firstLexerError :: Foldable t => t (L.Token L.Lexeme) -> Maybe ([L.Token L.Lexeme], L.Err)
firstLexerError (toList -> ts@((L.payload -> L.Err e) : _)) = Just (ts, e)
firstLexerError _ = Nothing

prettyParseError
  :: forall v
   . Var v
  => String
  -> Parser.Err v
  -> Pretty ColorText
prettyParseError s = \case
  P.TrivialError _ (LexerError ts (L.CloseWithoutMatchingOpen open close)) _ ->
    "❗️ I found a closing " <> style ErrorSite (fromString close) <>
    " here without a matching " <> style ErrorSite (fromString open) <> ".\n\n" <>
    showSource s ((\t -> (rangeForToken t, ErrorSite)) <$> ts)
  P.TrivialError sp unexpected expected
    -> fromString
        (P.parseErrorPretty @_ @Void (P.TrivialError sp unexpected expected))
      <> (case unexpected of
           Just (P.Tokens (toList -> ts)) -> case ts of
             [] -> mempty
             _ -> showSource s $ (\t -> (rangeForToken t, ErrorSite)) <$> ts
           _ -> mempty
         )
      <> lexerOutput
  P.FancyError _sp fancyErrors ->
    mconcat (go' <$> Set.toList fancyErrors) <> lexerOutput
 where
  go' :: P.ErrorFancy (Parser.Error v) -> Pretty ColorText
  go' (P.ErrorFail s) =
    "The parser failed with this message:\n" <> fromString s
  go' (P.ErrorIndentation ordering indent1 indent2) = mconcat
    [ "The parser was confused by the indentation.\n"
    , "It was expecting the reference level ("
    , fromString (show indent1)
    , ")\nto be "
    , fromString (show ordering)
    , " than/to the actual level ("
    , fromString (show indent2)
    , ").\n"
    ]
  go' (P.ErrorCustom e) = go e
  errorVar v = style ErrorSite . fromString . Text.unpack $ Var.name v
  go :: Parser.Error v -> Pretty ColorText
  -- | UseInvalidPrefixSuffix (Either (L.Token Name) (L.Token Name)) (Maybe [L.Token Name])
  go (Parser.UseEmpty tok) = msg where
    msg = Pr.indentN 2 . Pr.callout "😶" $ Pr.lines [
      Pr.wrap $ "I was expecting something after the " <> Pr.hiRed "use" <> "keyword", "",
      tokenAsErrorSite s tok,
      useExamples
      ]
  go (Parser.UseInvalidPrefixSuffix prefix suffix) = msg where
    msg :: Pretty ColorText
    msg = Pr.indentN 2 . Pr.blockedCallout . Pr.lines $ case (prefix, suffix) of
      (Left tok, Just _) -> [
        Pr.wrap "The first argument of a `use` statement can't be an operator name:", "",
        tokenAsErrorSite s tok,
        useExamples
        ]
      (tok0, Nothing) -> let tok = either id id tok0 in [
        Pr.wrap $ "I was expecting something after " <> Pr.hiRed "here:", "",
        tokenAsErrorSite s tok,
        case Name.parent (L.payload tok) of
          Nothing -> useExamples
          Just parent -> Pr.wrap $
            "You can write" <>
            Pr.group (Pr.blue $ "use " <> Pr.shown parent <> " "
                                       <> Pr.shown (Name.unqualified (L.payload tok))) <>
            "to introduce " <> Pr.backticked (Pr.shown (Name.unqualified (L.payload tok))) <>
            "as a local alias for " <> Pr.backticked (Pr.shown (L.payload tok))
        ]
      (Right tok, _) -> [ -- this is unpossible but rather than bomb, nice msg
        "You found a Unison bug 🐞  here:", "",
        tokenAsErrorSite s tok,
        Pr.wrap $
          "This looks like a valid `use` statement," <>
          "but the parser didn't recognize it. This is a Unison bug."
        ]
  go (Parser.DisallowedAbsoluteName t) = msg where
   msg :: Pretty ColorText
   msg = Pr.indentN 2 $ Pr.fatalCallout $ Pr.lines [
     Pr.wrap $ "I don't currently support creating definitions that start with"
           <> Pr.group (Pr.blue "'.'" <> ":"),
     "",
     tokenAsErrorSite s t,
     Pr.wrap $ "Use " <> Pr.blue "help messages.disallowedAbsolute" <> "to learn more.",
     ""
     ]
  go (Parser.DuplicateTypeNames ts) = intercalateMap "\n\n" showDup ts where
    showDup (v, locs) =
      "I found multiple types with the name " <> errorVar v <> ":\n\n" <>
      annotatedsStartingLineAsStyle ErrorSite s locs
  go (Parser.TypeDeclarationErrors es) = let
    unknownTypes = [ (v, a) | UF.UnknownType v a <- es ]
    dupDataAndAbilities = [ (v, a, a2) | UF.DupDataAndAbility v a a2 <- es ]
    unknownTypesMsg =
      mconcat [ "I don't know about the type(s) "
              , intercalateMap ", " errorVar (nubOrd $ fst <$> unknownTypes)
              , ":\n\n"
              , annotatedsAsStyle ErrorSite s (snd <$> unknownTypes)
              ]
    dupDataAndAbilitiesMsg = intercalateMap "\n\n" dupMsg dupDataAndAbilities
    dupMsg (v, a, a2) =
      mconcat [ "I found two types called " <> errorVar v <> ":"
              , "\n\n"
              , annotatedsStartingLineAsStyle ErrorSite s [a, a2]]
    in if null unknownTypes
       then dupDataAndAbilitiesMsg
       else if null dupDataAndAbilities then unknownTypesMsg
       else unknownTypesMsg <> "\n\n" <> dupDataAndAbilitiesMsg
  go (Parser.DidntExpectExpression _tok (Just t@(L.payload -> L.SymbolyId "::" Nothing)))
    = mconcat
      [ "This looks like the start of an expression here but I was expecting a binding."
      , "\nDid you mean to use a single " <> style Code ":"
      , " here for a type signature?"
      , "\n\n"
      , tokenAsErrorSite s t
      ]
  go (Parser.DidntExpectExpression tok _nextTok) = mconcat
    [ "This looks like the start of an expression here \n\n"
    , tokenAsErrorSite s tok
    , "\nbut at the file top-level, I expect one of the following:"
    , "\n"
    , "\n  - A binding, like " <> t <> style Code " = 42" <> " OR"
    , "\n                    " <> t <> style Code " : Nat"
    , "\n                    " <> t <> style Code " = 42"
    , "\n  - A watch expression, like " <> style Code "> " <> t <> style Code
                                                                         " + 1"
    , "\n  - An `ability` declaration, like "
      <> style Code "ability Foo where ..."
    , "\n  - A `type` declaration, like "
      <> style Code "type Optional a = None | Some a"
    , "\n  - A `namespace` declaration, like "
      <> style Code "namespace Seq where ..."
    , "\n"
    ]
    where t = style Code (fromString (P.showTokens (pure tok)))
  go (Parser.ExpectedBlockOpen blockName tok@(L.payload -> L.Close)) = mconcat
    [ "I was expecting an indented block following the " <>
      "`" <> fromString blockName <> "` keyword\n"
    , "but instead found an outdent:\n\n"
    , tokenAsErrorSite s tok ] -- todo: @aryairani why is this displaying weirdly?
  go (Parser.ExpectedBlockOpen blockName tok) = mconcat
    [ "I was expecting an indented block following the " <>
      "`" <> fromString blockName <> "` keyword\n"
    , "but instead found this token:\n"
    , tokenAsErrorSite s tok ]
  go (Parser.SignatureNeedsAccompanyingBody tok) = mconcat
    [ "You provided a type signature, but I didn't find an accompanying\n"
    , "binding after it.  Could it be a spelling mismatch?\n"
    , tokenAsErrorSite s tok
    ]
  go (Parser.EmptyBlock tok) = mconcat
    [ "I expected a block after this ("
    , describeStyle ErrorSite
    , "), "
    , "but there wasn't one.  Maybe check your indentation:\n"
    , tokenAsErrorSite s tok
    ]
  go Parser.EmptyWatch =
    "I expected a non-empty watch expression and not just \">\""
  go (Parser.UnknownAbilityConstructor tok _referents) = unknownConstructor "ability" tok
  go (Parser.UnknownDataConstructor    tok _referents) = unknownConstructor "data" tok
  go (Parser.UnknownId               tok referents references) = Pr.lines
    [ if missing then 
        "I couldn't resolve the reference " <> style ErrorSite (HQ.toString (L.payload tok)) <> "."
      else
        "The reference " <> style ErrorSite (HQ.toString (L.payload tok)) <> " was ambiguous."
    , ""
    , tokenAsErrorSite s $ HQ.toString <$> tok
    , if missing then "Make sure it's spelled correctly."
      else "Try hash-qualifying the term you meant to reference."
    ]
    where missing = Set.null referents && Set.null references
  go (Parser.UnknownTerm               tok referents) = Pr.lines
    [ if Set.null referents then 
        "I couldn't find a term for " <> style ErrorSite (HQ.toString (L.payload tok)) <> "."
      else
        "The term reference " <> style ErrorSite (HQ.toString (L.payload tok)) <> " was ambiguous."
    , ""
    , tokenAsErrorSite s $ HQ.toString <$> tok
    , if missing then "Make sure it's spelled correctly."
      else "Try hash-qualifying the term you meant to reference."
    ]
    where
    missing = Set.null referents
  go (Parser.UnknownType               tok referents) = Pr.lines
    [ if Set.null referents then 
        "I couldn't find a type for " <> style ErrorSite (HQ.toString (L.payload tok)) <> "."
      else
        "The type reference " <> style ErrorSite (HQ.toString (L.payload tok)) <> " was ambiguous."
    , ""
    , tokenAsErrorSite s $ HQ.toString <$> tok
    , if missing then "Make sure it's spelled correctly."
      else "Try hash-qualifying the type you meant to reference."
    ]
    where
    missing = Set.null referents
  go (Parser.ResolutionFailures        failures) =
    Pr.border 2 . prettyResolutionFailures s $ failures
  unknownConstructor
    :: String -> L.Token HashQualified -> Pretty ColorText
  unknownConstructor ctorType tok = Pr.lines [
    (Pr.wrap . mconcat) [ "I don't know about any "
    , fromString ctorType
    , " constructor named "
    , Pr.group (
        stylePretty ErrorSite (prettyHashQualified0 (L.payload tok)) <>
        "."
      )
    , "Maybe make sure it's correctly spelled and that you've imported it:"
    ]
    , ""
    , tokenAsErrorSite s tok
    ]
  lexerOutput :: Pretty (AnnotatedText a)
  lexerOutput = if showLexerOutput
    then "\nLexer output:\n" <> fromString (L.debugLex' s)
    else mempty

annotatedAsErrorSite
  :: Annotated a => String -> a -> Pretty ColorText
annotatedAsErrorSite = annotatedAsStyle ErrorSite

annotatedAsStyle
  :: (Ord style, Annotated a)
  => style
  -> String
  -> a
  -> Pretty (AnnotatedText style)
annotatedAsStyle style s ann =
  showSourceMaybes s [(, style) <$> rangeForAnnotated ann]

annotatedsAsErrorSite :: (Annotated a) => String -> [a] -> Pretty ColorText
annotatedsAsErrorSite = annotatedsAsStyle ErrorSite

annotatedsAsStyle :: (Annotated a) => Color -> String -> [a] -> Pretty ColorText
annotatedsAsStyle style src as =
  showSourceMaybes src [ (, style) <$> rangeForAnnotated a | a <- as ]

annotatedsStartingLineAsStyle
  :: (Annotated a) => Color -> String -> [a] -> Pretty ColorText
annotatedsStartingLineAsStyle style src as = showSourceMaybes
  src
  [ (, style) <$> (startingLine <$> rangeForAnnotated a) | a <- as ]

tokenAsErrorSite :: String -> L.Token a -> Pretty ColorText
tokenAsErrorSite src tok = showSource1 src (rangeForToken tok, ErrorSite)

tokensAsErrorSite :: String -> [L.Token a] -> Pretty ColorText
tokensAsErrorSite src ts =
  showSource src [(rangeForToken t, ErrorSite) | t <- ts ]

showSourceMaybes
  :: Ord a => String -> [Maybe (Range, a)] -> Pretty (AnnotatedText a)
showSourceMaybes src annotations = showSource src $ catMaybes annotations

showSource :: Ord a => String -> [(Range, a)] -> Pretty (AnnotatedText a)
showSource src annotations = Pr.lit . AT.condensedExcerptToText 6 $ AT.markup
  (fromString src)
  (Map.fromList annotations)

showSource1 :: Ord a => String -> (Range, a) -> Pretty (AnnotatedText a)
showSource1 src annotation = showSource src [annotation]

findTerm :: Seq (C.PathElement v loc) -> Maybe loc
findTerm = go
 where
  go (C.InSynthesize t        :<| _) = Just $ ABT.annotation t
  go (C.InCheck t _           :<| _) = Just $ ABT.annotation t
  go (C.InSynthesizeApp _ t _ :<| _) = Just $ ABT.annotation t
  go (_                       :<| t) = go t
  go Empty                           = Nothing

prettyTypecheckError
  :: (Var v, Ord loc, Show loc, Parser.Annotated loc)
  => C.ErrorNote v loc
  -> Env
  -> String
  -> Pretty ColorText
prettyTypecheckError = renderTypeError . typeErrorFromNote

prettyTypeInfo
  :: (Var v, Ord loc, Show loc, Parser.Annotated loc)
  => C.InfoNote v loc
  -> Env
  -> Pretty ColorText
prettyTypeInfo n e =
  maybe "" (`renderTypeInfo` e) (typeInfoFromNote n)

intLiteralSyntaxTip
  :: C.Term v loc -> C.Type v loc -> Pretty ColorText
intLiteralSyntaxTip term expectedType = case (term, expectedType) of
  (Term.Nat' n, Type.Ref' r) | r == Type.intRef ->
    "\nTip: Use the syntax "
      <> style Type2 ("+" <> show n)
      <> " to produce an "
      <> style Type2 "Int"
      <> "."
  _ -> ""

prettyResolutionFailures
  :: (Annotated a, Var v)
  => String
  -> [Names.ResolutionFailure v a]
  -> Pretty ColorText
prettyResolutionFailures s failures = Pr.callout "❓" $ Pr.linesNonEmpty
  [ Pr.wrap
    ("I couldn't resolve any of" <> style ErrorSite "these" <> "symbols:")
  , ""
  , annotatedsAsErrorSite s
  $  [ a | Names.TermResolutionFailure _ a _ <- failures ]
  ++ [ a | Names.TypeResolutionFailure _ a _ <- failures ]
  , let
      conflicts =
        nubOrd
          $  [ v
             | Names.TermResolutionFailure v _ s <- failures
             , Set.size s > 1
             ]
          ++ [ v
             | Names.TypeResolutionFailure v _ s <- failures
             , Set.size s > 1
             ]
      allVars =
        nubOrd
          $  [ v | Names.TermResolutionFailure v _ _ <- failures ]
          ++ [ v | Names.TypeResolutionFailure v _ _ <- failures ]
    in
      "Using these fully qualified names:"
      `Pr.hang` Pr.spaced (prettyVar <$> allVars)
      <>        "\n"
      <>        if null conflicts
                  then ""
                  else Pr.spaced (prettyVar <$> conflicts)
                    <> Pr.bold " are currently conflicted symbols"
  ]

useExamples :: Pretty ColorText
useExamples = Pr.lines [
  "Here's a few examples of valid `use` statements:", "",
  Pr.indentN 2 . Pr.column2 $
    [ (Pr.blue "use math sqrt", Pr.wrap "Introduces `sqrt` as a local alias for `math.sqrt`")
    , (Pr.blue "use List :+", Pr.wrap "Introduces `:+` as a local alias for `List.:+`.")
    , (Pr.blue "use .foo bar.baz", Pr.wrap "Introduces `bar.baz` as a local alias for the absolute name `.foo.bar.baz`") ]
  ]
