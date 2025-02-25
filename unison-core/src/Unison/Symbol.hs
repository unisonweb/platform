{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Symbol where

import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.Prelude
import Unison.Var (Var (..))
import Unison.Var qualified as Var

data Symbol = Symbol !Word64 Var.Type
  deriving stock (Generic, Eq, Ord)

instance ABT.Var Symbol where
  freshIn vs s | Set.null vs || Set.notMember s vs = s -- already fresh!
  freshIn vs s@(Symbol i n) = case Set.elemAt (Set.size vs - 1) vs of
    Symbol i2 _ -> if i > i2 then s else Symbol (i2 + 1) n

instance Var Symbol where
  typed t = Symbol 0 t
  typeOf (Symbol _ t) = t
  freshId (Symbol id _) = id
  freshenId id (Symbol _ n) = Symbol id n

instance Show Symbol where
  show (Symbol 0 n) = show n
  show (Symbol id n) = show n ++ "-" ++ show id

symbol :: Text -> Symbol
symbol = Var.named
