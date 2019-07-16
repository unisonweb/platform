{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Unison.TypePrinter where

import qualified Data.Map              as Map
import           Data.Maybe            (isJust)
import           Data.String           (fromString)
import           Unison.HashQualified  (HashQualified)
import           Unison.NamePrinter    (styleHashQualified'')
import           Unison.PrettyPrintEnv (PrettyPrintEnv, Imports, elideFQN)
import qualified Unison.PrettyPrintEnv as PrettyPrintEnv
import           Unison.Reference      (pattern Builtin)
import qualified Unison.SyntaxHighlights as S
import           Unison.SyntaxHighlights (fmt)
import           Unison.Type
import           Unison.Util.Pretty    (ColorText, Pretty)
import           Unison.Util.ColorText (toPlain)
import qualified Unison.Util.Pretty    as PP
import           Unison.Var            (Var)
import qualified Unison.Var            as Var
import qualified Unison.DataDeclaration as DD

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

pretty :: forall v a . (Var v) => PrettyPrintEnv -> Type v a -> Pretty ColorText
pretty ppe = pretty0 ppe mempty (-1)

pretty' :: Var v => Maybe Int -> PrettyPrintEnv -> Type v a -> String
pretty' (Just width) n t =
  toPlain $ PP.render width $ pretty0 n Map.empty (-1) t
pretty' Nothing n t = toPlain $ PP.render maxBound $ pretty0 n Map.empty (-1) t

pretty0
  :: forall v a . (Var v)
  => PrettyPrintEnv
  -> Imports
  -> Int
  -> Type v a
  -> Pretty ColorText
pretty0 n im p tp = prettyRaw n im p (cleanup (removePureEffects tp))

prettyRaw
  :: forall v a
   . (Var v)
  => PrettyPrintEnv
  -> Imports
  -> Int
  -> Type v a
  -> Pretty ColorText
-- p is the operator precedence of the enclosing context (a number from 0 to
-- 11, or -1 to avoid outer parentheses unconditionally).  Function
-- application has precedence 10.
prettyRaw n im p tp = go n im p tp
 where
  go :: PrettyPrintEnv -> Imports -> Int -> Type v a -> Pretty ColorText
  go n im p tp = case stripIntroOuters tp of
    Var' v -> fmt S.Var $ PP.text (Var.name v)
    -- Would be nice to use a different SyntaxHighlights
    -- color if the reference is an ability.
    Ref' r -> styleHashQualified'' (fmt S.DataType)
      $ elideFQN im (PrettyPrintEnv.typeName n r)
    Cycle' _ _ -> fromString "error: TypeParser does not currently emit Cycle"
    Abs' _     -> fromString "error: TypeParser does not currently emit Abs"
    Ann' _ _   -> fromString "error: TypeParser does not currently emit Ann"
    App' (Ref' (Builtin "Sequence")) x ->
      PP.group $ fmt S.DelimiterChar "[" <> go n im 0 x <> fmt S.DelimiterChar
                                                               "]"
    DD.TupleType' [x] ->
      PP.parenthesizeIf (p >= 10) $ fmt S.DataType "Pair" `PP.hang` PP.spaced
        [go n im 10 x, fmt S.DataType "()"]
    DD.TupleType' xs -> PP.parenthesizeCommas $ map (go n im 0) xs
    Apps' f xs -> PP.parenthesizeIf (p >= 10) $ go n im 9 f `PP.hang` PP.spaced
      (go n im 10 <$> xs)
    Effect1' e t ->
      PP.parenthesizeIf (p >= 10) $ go n im 9 e <> " " <> go n im 10 t
    Effects' es         -> effects (Just es)
    ForallNamed' v body ->
      paren (p >= 0)
      $         fmt S.TypeOperator "∀ "
      <>        fmt S.Var          (PP.text $ Var.name v)
      <>        fmt S.TypeOperator "."
      `PP.hang` go n im (-1) body
    t@(Arrow' _ _) -> case t of
      EffectfulArrows' (Ref' DD.UnitRef) rest -> arrows True True rest
      EffectfulArrows' fst rest ->
        PP.parenthesizeIf (p >= 0) $ go n im 0 fst <> arrows False False rest
      _ -> "error"
    _ -> "error"
  effects Nothing = mempty
  effects (Just es) =
    PP.group $ fmt S.AbilityBraces "{" <> PP.commas (go n im 0 <$> es) <> fmt
      S.AbilityBraces
      "}"
  arrow delay first mes =
    (if first then mempty else PP.softbreak <> fmt S.TypeOperator "->")
      <> (if delay
           then
             (if first
               then fmt S.DelayForceChar "'"
               else fmt S.DelayForceChar " '"
             )
           else mempty
         )
      <> effects mes
      <> if isJust mes || not delay && not first then " " else mempty
  arrows delay first [(mes, Ref' DD.UnitRef)] =
    arrow delay first mes <> fmt S.DataType "()"
  arrows delay first ((mes, Ref' DD.UnitRef) : rest) =
    arrow delay first mes <> parenNoGroup delay (arrows True True rest)
  arrows delay first ((mes, arg) : rest) =
    arrow delay first mes <> parenNoGroup
      (delay && not (null rest))
      (go n im 0 arg <> arrows False False rest)
  arrows False False [] = mempty
  arrows False True  [] = mempty  -- not reachable
  arrows True  _     [] = mempty  -- not reachable
  paren True  s = PP.group $ fmt S.Parenthesis "(" <> s <> fmt S.Parenthesis ")"
  paren False s = PP.group s
  parenNoGroup True  s = fmt S.Parenthesis "(" <> s <> fmt S.Parenthesis ")"
  parenNoGroup False s = s

-- todo: provide sample output in comment
prettySignatures'
  :: Var v
  => PrettyPrintEnv
  -> [(HashQualified, Type v a)]
  -> [Pretty ColorText]
prettySignatures' env ts = PP.align
  [ ( styleHashQualified'' (fmt S.DataType) name
    , (fmt S.TypeAscriptionColon ": " <> pretty0 env Map.empty (-1) typ)
      `PP.orElse` (  fmt S.TypeAscriptionColon ": "
                  <> PP.indentNAfterNewline 2 (pretty0 env Map.empty (-1) typ)
                  )
    )
  | (name, typ) <- ts
  ]

-- todo: provide sample output in comment; different from prettySignatures'
prettySignaturesAlt'
  :: Var v
  => PrettyPrintEnv
  -> [([HashQualified], Type v a)]
  -> [Pretty ColorText]
prettySignaturesAlt' env ts = PP.align
  [ ( PP.commas . fmap (styleHashQualified'' (fmt S.DataType)) $ names
    , (fmt S.TypeAscriptionColon ": " <> pretty0 env Map.empty (-1) typ)
      `PP.orElse` (  fmt S.TypeAscriptionColon ": "
                  <> PP.indentNAfterNewline 2 (pretty0 env Map.empty (-1) typ)
                  )
    )
  | (names, typ) <- ts
  ]

prettySignatures
  :: Var v
  => PrettyPrintEnv
  -> [(HashQualified, Type v a)]
  -> Pretty ColorText
prettySignatures env ts = PP.lines $
  PP.group <$> prettySignatures' env ts

prettySignaturesAlt
  :: Var v
  => PrettyPrintEnv
  -> [([HashQualified], Type v a)]
  -> Pretty ColorText
prettySignaturesAlt env ts = PP.lines $
  PP.group <$> prettySignaturesAlt' env ts
