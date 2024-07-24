module Unison.PatternMatchCoverage.Constraint
  ( Constraint (..),
    prettyConstraint,
  )
where

import Unison.ConstructorReference (ConstructorReference)
import Unison.PatternMatchCoverage.EffectHandler
import Unison.PatternMatchCoverage.IntervalSet (IntervalSet)
import Unison.PatternMatchCoverage.PmLit
import Unison.PatternMatchCoverage.Pretty
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.Syntax.TypePrinter qualified as TypePrinter
import Unison.Type (Type)
import Unison.Util.Pretty
import Unison.Var (Var)

-- | A constraint to add to a [normalized constraint
-- set]("Unison.PatternMatchCoverage.NormalizedConstraints") (fig 6)
-- See 'Unison.PatternMatchCoverage.Solve.addConstraint'
data Constraint vt v loc
  = -- | Positive constraint regarding data type. States that the
    -- given variable must be the given constructor, and it also binds
    -- variables corresponding to constructor arguments.
    PosCon v ConstructorReference [(v, Type vt loc)]
  | -- | Negative constraint concerning data type. States that the
    -- given variable must not be the given constructor.
    NegCon v ConstructorReference
  | -- | Positive constraint regarding data type. States that the
    -- given variable must be the given constructor, and it also binds
    -- variables corresponding to constructor arguments.
    PosEffect v EffectHandler [(v, Type vt loc)]
  | -- | Negative constraint concerning data type. States that the
    -- given variable must not be the given constructor.
    NegEffect v EffectHandler
  | -- | Positive constraint regarding literal
    PosLit v PmLit
  | -- | Negative constraint regarding literal
    NegLit v PmLit
  | -- | Positive constraint on list element with position relative to head of list
    PosListHead
      -- | list root
      v
      -- | cons position (0 is head)
      Int
      -- | element variable
      v
  | -- | Positive constraint on list element with position relative to end of list
    PosListTail
      -- | list root
      v
      -- | snoc position (0 is last)
      Int
      -- | element variable
      v
  | -- | Negative constraint on length of the list (/i.e./ the list
    -- may not be an element of the interval set)
    NegListInterval v IntervalSet
  | -- | An effect is performed
    Effectful v
  | -- | Equality constraint
    Eq v v
  deriving stock (Eq, Ord)

prettyConstraint :: forall vt v loc. (Var vt, Var v) => PrettyPrintEnv -> Constraint vt v loc -> Pretty ColorText
prettyConstraint ppe = \case
  PosCon var con convars ->
    let xs = pc con : fmap (\(trm, typ) -> sep " " ["(" <> prettyVar trm, ":", TypePrinter.pretty ppe typ <> ")"]) convars ++ ["<-", prettyVar var]
     in sep " " xs
  NegCon var con -> sep " " [prettyVar var, "≠", pc con]
  PosEffect var eff effvars ->
    let xs = pe eff : fmap (\(trm, typ) -> sep " " ["(" <> prettyVar trm, ":", TypePrinter.pretty ppe typ <> ")"]) effvars ++ ["<-", prettyVar var]
     in sep " " xs
  NegEffect var eff -> sep " " [prettyVar var, "≠", pe eff]
  PosLit var lit -> sep " " [prettyPmLit lit, "<-", prettyVar var]
  NegLit var lit -> sep " " [prettyVar var, "≠", prettyPmLit lit]
  PosListHead root n el -> sep " " [prettyVar el, "<-", "head", pany n, prettyVar root]
  PosListTail root n el -> sep " " [prettyVar el, "<-", "tail", pany n, prettyVar root]
  NegListInterval var x -> sep " " [prettyVar var, "≠", string (show x)]
  Effectful var -> "!" <> prettyVar var
  Eq v0 v1 -> sep " " [prettyVar v0, "=", prettyVar v1]
  where
    pany :: (Show a) => a -> Pretty ColorText
    pany = string . show

    pc = prettyConstructorReference ppe
    pe = prettyEffectHandler ppe
