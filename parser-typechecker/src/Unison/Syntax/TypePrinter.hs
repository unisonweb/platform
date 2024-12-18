{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Syntax.TypePrinter
  ( pretty,
    pretty0,
    prettyRaw,
    prettyStr,
    prettySyntax,
    prettySignaturesST,
    prettySignaturesCT,
    prettySignaturesCTCollapsed,
    prettySignaturesAlt,
    prettySignaturesAlt',
    runPretty,
  )
where

import Control.Monad.Reader (ask)
import Data.Map qualified as Map
import Unison.Builtin.Decls qualified as DD
import Unison.HashQualified (HashQualified)
import Unison.Name (Name)
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.PrettyPrintEnv qualified as PrettyPrintEnv
import Unison.PrettyPrintEnv.FQN (Imports, elideFQN)
import Unison.PrettyPrintEnv.MonadPretty (Env (..), MonadPretty, runPretty, willCaptureType)
import Unison.Reference (Reference, pattern Builtin)
import Unison.Referent (Referent)
import Unison.Settings qualified as Settings
import Unison.Syntax.NamePrinter (styleHashQualified'')
import Unison.Type
import Unison.Util.ColorText (toPlain)
import Unison.Util.Pretty (ColorText, Pretty, Width)
import Unison.Util.Pretty qualified as PP
import Unison.Util.SyntaxText qualified as S
import Unison.Var (Var)
import Unison.Var qualified as Var

type SyntaxText = S.SyntaxText' Reference

pretty :: (Var v) => PrettyPrintEnv -> Type v a -> Pretty ColorText
pretty ppe t = PP.syntaxToColor $ prettySyntax ppe t

prettySyntax :: (Var v) => PrettyPrintEnv -> Type v a -> Pretty SyntaxText
prettySyntax ppe = runPretty ppe . pretty0 Map.empty (-1)

prettyStr :: (Var v) => Maybe Width -> PrettyPrintEnv -> Type v a -> String
prettyStr (Just width) ppe t =
  toPlain . PP.render width . PP.syntaxToColor . runPretty ppe $ pretty0 Map.empty (-1) t
prettyStr Nothing ppe t =
  toPlain . PP.render maxBound . PP.syntaxToColor . runPretty ppe $ pretty0 Map.empty (-1) t

{- Explanation of precedence handling

   We illustrate precedence rules as follows.

     >=10
       10f 10x

   This example shows that a type application f x is enclosed in parentheses
   whenever the ambient precedence around it is >= 10, and that when printing
   its two components, an ambient precedence of 10 is used in both places.

   The pretty-printer uses the following rules for printing types.

     >=10
       10f 10x
       { 0e } 10t

     >=0
       0a -> 0b

-}

pretty0 ::
  forall v a m.
  (MonadPretty v m) =>
  Imports ->
  Int ->
  Type v a ->
  m (Pretty SyntaxText)
pretty0 im p tp = prettyRaw im p (removeEmptyEffects $ cleanup tp)

prettyRaw ::
  forall v a m.
  (MonadPretty v m) =>
  Imports ->
  Int ->
  Type v a ->
  m (Pretty SyntaxText)
-- p is the operator precedence of the enclosing context (a number from 0 to
-- 11, or -1 to avoid outer parentheses unconditionally).  Function
-- application has precedence 10.
prettyRaw im p tp = go im p tp
  where
    go :: Imports -> Int -> Type v a -> m (Pretty SyntaxText)
    go im p tp = case stripIntroOuters tp of
      Var' v -> pure . fmt S.Var $ PP.text (Var.name v)
      DD.TupleType' xs | length xs /= 1 -> PP.parenthesizeCommas <$> traverse (go im 0) xs
      -- Would be nice to use a different SyntaxHighlights color if the reference is an ability.
      Ref' r -> do
        env <- ask
        pure $ styleHashQualified'' (fmt $ S.TypeReference r) $ elideFQN im (PrettyPrintEnv.typeName env.ppe r)
      Cycle' _ _ -> pure $ fromString "bug: TypeParser does not currently emit Cycle"
      Abs' _ -> pure $ fromString "bug: TypeParser does not currently emit Abs"
      Ann' _ _ -> pure $ fromString "bug: TypeParser does not currently emit Ann"
      App' (Ref' (Builtin "Sequence")) x -> do
        x' <- go im (-1) x
        pure $ PP.group $ fmt S.DelimiterChar "[" <> x' <> fmt S.DelimiterChar "]"
      Apps' f xs ->
        PP.parenthesizeIf (p >= 10)
          <$> ( PP.hang <$> go im 9 f <*> (PP.spaced <$> traverse (go im 10) xs)
              )
      Effect1' e t ->
        PP.parenthesizeIf (p >= 10) <$> ((\x y -> x <> " " <> y) <$> go im 9 e <*> go im 10 t)
      Effects' es -> effects (Just es)
      ForallsNamed' vs' body ->
        let vs = filter (\v -> Var.name v /= "()") vs'
            prettyForall p = do
              let vformatted = PP.sep " " (fmt S.Var . PP.text . Var.name <$> vs)
              PP.hang (fmt S.TypeOperator "∀ " <> vformatted <> fmt S.TypeOperator ".") <$> go im p body
         in -- if we're printing a type signature, and all the type variables
            -- are universally quantified, then we can omit the `forall` keyword
            -- only if the type variables are not bound in an outer scope
            if p < 0 && not Settings.debugRevealForalls && all Var.universallyQuantifyIfFree vs
              then ifM (willCaptureType vs) (prettyForall p) (go im p body)
              else paren (p >= 0) <$> prettyForall (-1)
      t@(Arrow' _ _) -> case t of
        EffectfulArrows' (Ref' DD.UnitRef) rest ->
          PP.parenthesizeIf (p >= 10) <$> arrows True True rest
        EffectfulArrows' fst rest ->
          case fst of
            Var' v
              | Var.name v == "()" ->
                  PP.parenthesizeIf (p >= 10) <$> arrows True True rest
            _ ->
              PP.parenthesizeIf (p >= 0)
                <$> ((<>) <$> go im 0 fst <*> arrows False False rest)
        _ -> pure . fromString $ "bug: unexpected Arrow form in prettyRaw: " <> show t
      _ -> pure . fromString $ "bug: unexpected form in prettyRaw: " <> show tp
    effects Nothing = pure mempty
    effects (Just es) =
      PP.group . (fmt S.AbilityBraces "{" <>) . (<> fmt S.AbilityBraces "}")
        <$> (PP.commas <$> traverse (go im 0) es)
    -- `first`: is this the first argument?
    -- `mes`: list of effects
    arrow delay first mes = do
      es <- effects mes
      pure $
        (if first then mempty else PP.softbreak <> fmt S.TypeOperator "->")
          <> (if delay then (if first then fmt S.DelayForceChar "'" else fmt S.DelayForceChar " '") else mempty)
          <> es
          <> if isJust mes || not delay && not first then " " else mempty

    arrows ::
      Bool ->
      Bool ->
      [(Maybe [Type v a], Type v a)] ->
      m (Pretty SyntaxText)
    arrows delay first [(mes, Ref' DD.UnitRef)] = (<> fmt S.Unit "()") <$> arrow delay first mes
    arrows delay first ((mes, Ref' DD.UnitRef) : rest) = do
      es <- arrow delay first mes
      rest' <- arrows True True rest
      pure $ es <> parenNoGroup delay rest'
    arrows delay first ((mes, arg) : rest) = do
      es <- arrow delay first mes
      arg' <- go im 0 arg
      rest' <- arrows False False rest
      pure $ es <> parenNoGroup (delay && not (null rest)) (arg' <> rest')
    arrows False False [] = pure mempty
    arrows False True [] = pure mempty -- not reachable
    arrows True _ [] = pure mempty -- not reachable
    paren True s = PP.group $ fmt S.Parenthesis "(" <> s <> fmt S.Parenthesis ")"
    paren False s = PP.group s

    parenNoGroup True s = fmt S.Parenthesis "(" <> s <> fmt S.Parenthesis ")"
    parenNoGroup False s = s

fmt :: S.Element r -> Pretty (S.SyntaxText' r) -> Pretty (S.SyntaxText' r)
fmt = PP.withSyntax

-- todo: provide sample output in comment
prettySignaturesCT ::
  (Var v) =>
  PrettyPrintEnv ->
  [(Referent, HashQualified Name, Type v a)] ->
  [Pretty ColorText]
prettySignaturesCT ppe ts = map PP.syntaxToColor $ prettySignaturesST ppe ts

prettySignaturesCTCollapsed ::
  (Var v) =>
  PrettyPrintEnv ->
  [(Referent, HashQualified Name, Type v a)] ->
  Pretty ColorText
prettySignaturesCTCollapsed ppe ts =
  PP.lines
    . map PP.group
    $ prettySignaturesCT ppe ts

prettySignaturesST ::
  (Var v) =>
  PrettyPrintEnv ->
  [(Referent, HashQualified Name, Type v a)] ->
  [Pretty SyntaxText]
prettySignaturesST ppe ts =
  PP.align . runPretty ppe $ traverse (\(r, hq, typ) -> (name r hq,) <$> sig typ) ts
  where
    name r hq =
      styleHashQualified'' (fmt $ S.TermReference r) hq
    sig typ = do
      t <- pretty0 Map.empty (-1) typ
      let col = fmt S.TypeAscriptionColon ": "
      pure $ (col <> t) `PP.orElse` (col <> PP.indentNAfterNewline 2 t)

-- todo: provide sample output in comment; different from prettySignatures'
prettySignaturesAlt' ::
  (Var v) =>
  PrettyPrintEnv ->
  [([HashQualified Name], Type v a)] ->
  [Pretty ColorText]
prettySignaturesAlt' ppe ts = runPretty ppe $
  do
    ts' <- traverse f ts
    pure $ map PP.syntaxToColor $ PP.align ts'
  where
    f :: (MonadPretty v m) => ([HashQualified Name], Type v a) -> m (Pretty SyntaxText, Pretty SyntaxText)
    f (names, typ) = do
      typ' <- pretty0 Map.empty (-1) typ
      let col = fmt S.TypeAscriptionColon ": "
      pure
        ( PP.commas . fmap (\name -> styleHashQualified'' (fmt $ S.HashQualifier name) name) $ names,
          (col <> typ') `PP.orElse` (col <> PP.indentNAfterNewline 2 typ')
        )

-- prettySignatures'' :: Var v => [(Name, Type v a)] -> [Pretty ColorText]
-- prettySignatures'' env ts = prettySignatures' env (first HQ.fromName <$> ts)

prettySignaturesAlt ::
  (Var v) =>
  PrettyPrintEnv ->
  [([HashQualified Name], Type v a)] ->
  Pretty ColorText
prettySignaturesAlt ppe ts =
  PP.lines
    . map PP.group
    $ prettySignaturesAlt' ppe ts
