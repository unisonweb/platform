{-# LANGUAGE OverloadedStrings #-}
module Unison.Builtin where

import           Control.Arrow ((&&&))
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import           Unison.Parser (penv0)
import           Unison.Parsers (unsafeParseType, unsafeParseTerm)
import qualified Unison.Reference as R
import           Unison.Symbol (Symbol)
import           Unison.Term (Term)
import qualified Unison.Term as Term
import           Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Var as Var
import           Unison.Var (Var)

t :: String -> Type Symbol
t s = resolveBuiltinTypes $ unsafeParseType s penv0

resolveBuiltinTypes :: Type Symbol -> Type Symbol
resolveBuiltinTypes t =
  let free = Set.intersection (ABT.freeVars t) builtinTypes
  in ABT.substs [(v, Type.builtin (Var.name v)) | v <- Set.toList free ] t

tm :: String -> Term Symbol
tm s = let
  t = unsafeParseTerm s penv0
  free = Set.intersection (ABT.freeVars t) builtinTerms
  in ABT.substs [(v, Term.builtin (Var.name v)) | v <- Set.toList free ] t

builtinTypes :: Set Symbol
builtinTypes = Set.fromList . map Var.named $ [
  "Int64", "UInt64", "Float", "Boolean", "Sequence", "Text", "Stream"]

builtinTerms :: Set Symbol
builtinTerms = Set.map toSymbol (Map.keysSet builtins) where

builtinEnv :: [(Symbol, Term Symbol)]
builtinEnv = (toSymbol &&& Term.ref) <$> Map.keys builtins

toSymbol :: Var v => R.Reference -> v
toSymbol (R.Builtin txt) = Var.named txt
toSymbol _ = error "unpossible"

builtins :: Map.Map R.Reference (Type Symbol)
builtins = Map.fromList $
  [ (R.Builtin name, t typ) |
    (name, typ) <-
      [ ("Int64.+", "Int64 -> Int64 -> Int64")
      , ("Int64.-", "Int64 -> Int64 -> Int64")
      , ("Int64.*", "Int64 -> Int64 -> Int64")
      , ("Int64./", "Int64 -> Int64 -> Int64")
      , ("Int64.<", "Int64 -> Int64 -> Boolean")
      , ("Int64.>", "Int64 -> Int64 -> Boolean")
      , ("Int64.<=", "Int64 -> Int64 -> Boolean")
      , ("Int64.>=", "Int64 -> Int64 -> Boolean")
      , ("Int64.==", "Int64 -> Int64 -> Boolean")
      , ("Int64.increment", "Int64 -> Int64")
      , ("Int64.is-even", "Int64 -> Boolean")
      , ("Int64.is-odd", "Int64 -> Boolean")
      , ("Int64.signum", "Int64 -> Int64")
      , ("Int64.negate", "Int64 -> Int64")

      , ("UInt64.+", "UInt64 -> UInt64 -> UInt64")
      , ("UInt64.drop", "UInt64 -> UInt64 -> UInt64")
      , ("UInt64.sub", "UInt64 -> UInt64 -> Int64")
      , ("UInt64.*", "UInt64 -> UInt64 -> UInt64")
      , ("UInt64./", "UInt64 -> UInt64 -> UInt64")
      , ("UInt64.<", "UInt64 -> UInt64 -> Boolean")
      , ("UInt64.>", "UInt64 -> UInt64 -> Boolean")
      , ("UInt64.<=", "UInt64 -> UInt64 -> Boolean")
      , ("UInt64.>=", "UInt64 -> UInt64 -> Boolean")
      , ("UInt64.==", "UInt64 -> UInt64 -> Boolean")
      , ("UInt64.increment", "UInt64 -> UInt64")
      , ("UInt64.is-even", "UInt64 -> Boolean")
      , ("UInt64.is-odd", "UInt64 -> Boolean")

      , ("Float.+", "Float -> Float -> Float")
      , ("Float.-", "Float -> Float -> Float")
      , ("Float.*", "Float -> Float -> Float")
      , ("Float./", "Float -> Float -> Float")
      , ("Float.<", "Float -> Float -> Boolean")
      , ("Float.>", "Float -> Float -> Boolean")
      , ("Float.<=", "Float -> Float -> Boolean")
      , ("Float.>=", "Float -> Float -> Boolean")
      , ("Float.==", "Float -> Float -> Boolean")

      , ("Boolean.not", "Boolean -> Boolean")

      , ("Text.empty", "Text")
      , ("Text.concatenate", "Text -> Text -> Text")
      , ("Text.take", "UInt64 -> Text -> Text")
      , ("Text.drop", "UInt64 -> Text -> Text")
      , ("Text.size", "Text -> UInt64")
      , ("Text.==", "Text -> Text -> Boolean")
      , ("Text.!=", "Text -> Text -> Boolean")
      , ("Text.<=", "Text -> Text -> Boolean")
      , ("Text.>=", "Text -> Text -> Boolean")
      , ("Text.<", "Text -> Text -> Boolean")
      , ("Text.>", "Text -> Text -> Boolean")

      , ("Stream.empty", "forall a . Stream a")
      , ("Stream.from-int64", "Int64 -> Stream Int64")
      , ("Stream.cons", "forall a . a -> Stream a -> Stream a")
      , ("Stream.take", "forall a . UInt64 -> Stream a -> Stream a")
      , ("Stream.drop", "forall a . UInt64 -> Stream a -> Stream a")
      , ("Stream.map", "forall a b . (a -> b) -> Stream a -> Stream b")
      , ("Stream.fold-left", "forall a b . b -> (b -> a -> b) -> Stream a -> b")

      , ("Sequence.empty", "forall a . Sequence a")
      , ("Sequence.cons", "forall a . a -> Sequence a -> Sequence a")
      , ("Sequence.snoc", "forall a . Sequence a -> a -> Sequence a")
      , ("Sequence.take", "forall a . UInt64 -> Sequence a -> Sequence a")
      , ("Sequence.size", "forall a . Sequence a -> UInt64")
      ]
  ]
