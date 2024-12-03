module Unison.Syntax.Precedence where

import Data.Map qualified as Map
import Unison.Prelude

-- Precedence rules for infix operators.
-- Lower number means higher precedence (tighter binding).
-- Operators not in this list have no precedence and will simply be parsed
-- left-to-right.
infixRules :: Map Text Precedence
infixRules =
  Map.fromList do
    (ops, prec) <- zip infixLevels (map (InfixOp . Level) [0 ..])
    map (,prec) ops

-- | Indicates this is the RHS of a top-level definition.
isTopLevelPrecedence :: Precedence -> Bool
isTopLevelPrecedence i = i == Basement

increment :: Precedence -> Precedence
increment = \case
  Basement -> Bottom
  Bottom -> Annotation
  Annotation -> Statement
  Statement -> Control
  Control -> InfixOp Lowest
  InfixOp Lowest -> InfixOp (Level 0)
  InfixOp (Level n) -> InfixOp (Level (n + 1))
  InfixOp Highest -> Application
  Application -> Prefix
  Prefix -> Top
  Top -> Top

data Precedence
  = -- | The lowest precedence, used for top-level bindings
    Basement
  | -- | Used for terms that never need parentheses
    Bottom
  | -- | Type annotations
    Annotation
  | -- | A statement in a block
    Statement
  | -- | Control flow constructs like `if`, `match`, `case`
    Control
  | -- | Infix operators
    InfixOp InfixPrecedence
  | -- | Function application
    Application
  | -- | Prefix operators like `'`, `!`
    Prefix
  | -- | The highest precedence, used for let bindings and blocks
    Top
  deriving (Eq, Ord, Show)

data InfixPrecedence = Lowest | Level Int | Highest
  deriving (Eq, Ord, Show)

infixLevels :: [[Text]]
infixLevels =
  [ ["||", "|"],
    ["&&", "&"],
    ["==", "!==", "!=", "==="],
    ["<", ">", ">=", "<="],
    ["+", "-"],
    ["*", "/", "%"],
    ["^", "^^", "**"]
  ]

-- | Returns the precedence of an infix operator, if it has one.
operatorPrecedence :: Text -> Maybe Precedence
operatorPrecedence op = Map.lookup op infixRules
