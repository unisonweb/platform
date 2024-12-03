module Unison.LSP.FileAnalysis.UnusedBindings where

import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Language.LSP.Protocol.Types (Diagnostic)
import Language.LSP.Protocol.Types qualified as Lsp
import U.Core.ABT (ABT (..))
import U.Core.ABT qualified as ABT
import Unison.LSP.Conversions qualified as Cv
import Unison.LSP.Diagnostics qualified as Diagnostic
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Symbol (Symbol (..))
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Util.List qualified as ListUtils
import Unison.Util.Range qualified as Range
import Unison.Util.Recursion
import Unison.Var qualified as Var

data VarUsages = VarUsages
  { unusedVars :: Map Symbol (Set Ann),
    usedVars :: Set Symbol,
    -- This is generally a copy of usedVars, except that we _don't_ remove variables when they go out of scope.
    -- This is solely so we have the information to handle an edge case in pattern guards where vars are independently
    -- brought into scope in BOTH the guards and the body of a match case, and we want to count a var as used if it
    -- appears in _either_.
    allUsedVars :: Set Symbol
  }

instance Semigroup VarUsages where
  VarUsages a b c <> VarUsages a' b' c' =
    VarUsages (Map.unionWith (<>) a a') (b <> b') (c <> c')

instance Monoid VarUsages where
  mempty = VarUsages mempty mempty mempty

analyseTerm :: Lsp.Uri -> Term Symbol Ann -> [Diagnostic]
analyseTerm fileUri tm =
  let (VarUsages {unusedVars}) = cata alg tm
      vars =
        Map.toList unusedVars & mapMaybe \(v, ann) -> do
          (,ann) <$> getRelevantVarName v
      diagnostics =
        vars & foldMap \(varName, anns) -> do
          ann <- Set.toList anns
          range <- maybeToList $ Cv.annToURange ann
          -- Limit the range to the first line of the binding to not be too annoying.
          -- Maybe in the future we can get the actual annotation of the variable name.
          let lspRange = Cv.uToLspRange . Range.startingLine $ range
          pure $ Diagnostic.mkDiagnostic fileUri lspRange Diagnostic.DiagnosticSeverity_Warning [Lsp.DiagnosticTag_Unnecessary] ("Unused binding " <> tShow varName <> ". Use the binding, or prefix it with an _ to dismiss this warning.") []
   in diagnostics
  where
    getRelevantVarName :: Symbol -> Maybe Text
    getRelevantVarName = \case
      -- Sometimes 'do' gets a binding of '()', which we don't care about
      Symbol _ (Var.User "()") -> Nothing
      Symbol _ (Var.User "") -> Nothing
      -- We only care about user bindings which don't start with an underscore
      Symbol _ (Var.User n) -> do
        guard (not (Text.isPrefixOf "_" n))
        Just n
      _ -> Nothing
    alg :: Algebra (ABT.Term' (Term.F Symbol Ann Ann) Symbol Ann) VarUsages
    alg (ABT.Term' _ ann abt) = case abt of
      Var v -> VarUsages {unusedVars = mempty, usedVars = Set.singleton v, allUsedVars = Set.singleton v}
      Cycle x -> x
      Abs v (VarUsages {unusedVars, usedVars, allUsedVars}) ->
        if v `Set.member` usedVars
          then VarUsages {unusedVars, usedVars = Set.delete v usedVars, allUsedVars}
          else VarUsages {unusedVars = Map.insert v (Set.singleton ann) unusedVars, usedVars, allUsedVars}
      Tm fx ->
        case fx of
          -- We need to special-case pattern guards because the pattern, guard, and body treat each of their vars in
          -- their own independent scopes, even though the vars created in the pattern are the same ones used in the
          -- guards and bindings :shrug:
          Term.Match scrutinee cases ->
            let -- There's a separate case for every guard on a single pattern, so we first do our best to group up cases with the same pattern.
                -- Otherwise, a var may be reported unused in one branch of a guard even though it's used in another branch.
                groupedCases = ListUtils.groupBy (\(Term.MatchCase pat _ _) -> pat) cases
                caseVars =
                  groupedCases & foldMap \singlePatCases ->
                    let (VarUsages {unusedVars = unused, usedVars = used, allUsedVars = allUsed}) =
                          singlePatCases
                            & foldMap
                              ( \(Term.MatchCase pat guard body) ->
                                  -- This is imprecise, but it's quite annoying to get the actual ann of the unused bindings, so
                                  -- we just use the FULL span of the pattern for now. We could fix this with a bit
                                  -- of elbow grease.
                                  let patSpanAnn = fold pat
                                      combindedVarUsages = fold guard <> body
                                   in combindedVarUsages {unusedVars = (unusedVars combindedVarUsages) $> (Set.singleton patSpanAnn)}
                              )
                        actuallyUnusedVars = unused & Map.filterWithKey \k _ -> k `Set.notMember` allUsed
                     in VarUsages {unusedVars = actuallyUnusedVars, usedVars = used, allUsedVars = allUsed}
             in scrutinee <> caseVars
          _ -> Foldable.fold fx
