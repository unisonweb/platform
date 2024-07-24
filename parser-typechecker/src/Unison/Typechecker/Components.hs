module Unison.Typechecker.Components (minimize, minimize') where

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as Nel
import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.Prelude
import Unison.Term (Term')
import Unison.Term qualified as Term
import Unison.Var (Var)
import Unison.Var qualified as Var

unordered :: (Var v) => [(v, Term' vt v a)] -> [[(v, Term' vt v a)]]
unordered = ABT.components

ordered :: (Var v) => [(v, Term' vt v a)] -> [[(v, Term' vt v a)]]
ordered = ABT.orderedComponents

-- | Algorithm for minimizing cycles of a `let rec`. This can
-- improve generalization during typechecking and may also be more
-- efficient for execution.
--
-- For instance:
--
-- minimize (let rec id x = x; g = id 42; y = id "hi" in g)
-- ==>
-- Just (let id x = x; g = id 42; y = id "hi" in g)
--
-- Gets rid of the let rec and replaces it with an ordinary `let`, such
-- that `id` is suitably generalized.
--
-- Fails on the left if there are duplicate definitions.
minimize ::
  (Var v) =>
  Term' vt v a ->
  Either (NonEmpty (v, [a])) (Maybe (Term' vt v a))
minimize (Term.LetRecNamedAnnotatedTop' isTop blockAnn bs e) =
  let bindings = first snd <$> bs
      group =
        map (fst . head &&& map (ABT.annotation . snd))
          . groupBy ((==) `on` fst)
          . sortBy
            (compare `on` fst)
      grouped = group bindings
      dupes = filter ok grouped
        where
          ok (v, as)
            | Var.name v == "_" = False
            | otherwise = length as > 1
   in if not $ null dupes
        then Left $ Nel.fromList dupes
        else
          let cs0 = if isTop then unordered bindings else ordered bindings
              -- within a cycle, we put the lambdas first, so
              -- unguarded definitions can refer to these lambdas, example:
              --
              --   foo x = blah + 1 + x
              --   blah = foo 10
              --
              -- Here `foo` and `blah` are part of a cycle, but putting `foo`
              -- first at least lets the program run (though it has an infinite
              -- loop).
              cs = sortOn (\(_, e) -> Term.arity e == 0) <$> cs0
              varAnnotations = Map.fromList ((\((a, v), _) -> (v, a)) <$> bs)
              msg v = error $ "Components.minimize " <> show (v, Map.keys varAnnotations)
              annotationFor v = fromMaybe (msg v) $ Map.lookup v varAnnotations
              annotatedVar v = (annotationFor v, v)
              -- When introducing a nested let/let rec, we use the annotation
              -- of the variable that starts off that let/let rec
              mklet [(hdv, hdb)] e
                | Set.member hdv (ABT.freeVars hdb) =
                    Term.letRec
                      isTop
                      blockAnn
                      [(annotatedVar hdv, hdb)]
                      e
                | otherwise = Term.singleLet isTop blockAnn (annotationFor hdv) (hdv, hdb) e
              mklet cycle@((_, _) : _) e =
                Term.letRec
                  isTop
                  blockAnn
                  (first annotatedVar <$> cycle)
                  e
              mklet [] e = e
           in Right . Just . foldr mklet e $ cs
minimize _ = Right Nothing

minimize' ::
  (Var v) => Term' vt v a -> Either (NonEmpty (v, [a])) (Term' vt v a)
minimize' term = fromMaybe term <$> minimize term
