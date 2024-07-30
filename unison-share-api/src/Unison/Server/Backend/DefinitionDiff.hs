-- | Utilities for displaying diffs between definitions.
module Unison.Server.Backend.DefinitionDiff
  ( diffDisplayObjects,
  )
where

import Data.Algorithm.Diff qualified as Diff
import Data.Foldable qualified as Foldable
import Data.Function
import Data.List qualified as List
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import Unison.Prelude
import Unison.Server.Syntax (SyntaxText)
import Unison.Server.Syntax qualified as Syntax
import Unison.Server.Types (DisplayObjectDiff (..), SemanticSyntaxDiff (..))
import Unison.Util.AnnotatedText (AnnotatedText (..))
import Unison.Util.AnnotatedText qualified as AT

diffDisplayObjects :: (HasCallStack) => DisplayObject SyntaxText SyntaxText -> DisplayObject SyntaxText SyntaxText -> DisplayObjectDiff
diffDisplayObjects from to = case (from, to) of
  (BuiltinObject fromST, BuiltinObject toST) -> DisplayObjectDiff (BuiltinObject (diffSyntaxText fromST toST))
  (MissingObject fromSH, MissingObject toSH)
    | fromSH == toSH -> DisplayObjectDiff (MissingObject fromSH)
    | otherwise -> MismatchedDisplayObjects (MissingObject fromSH) (MissingObject toSH)
  (UserObject fromST, UserObject toST) -> DisplayObjectDiff (UserObject (diffSyntaxText fromST toST))
  (l, r) -> MismatchedDisplayObjects l r

diffSyntaxText :: SyntaxText -> SyntaxText -> [SemanticSyntaxDiff]
diffSyntaxText (AnnotatedText fromST) (AnnotatedText toST) =
  Diff.getGroupedDiffBy
    diffEq
    (Foldable.toList @Seq fromST)
    (Foldable.toList @Seq toST)
    & expandSpecialCases
  where
    -- We special-case situations where the name of a definition changed but its hash didn't;
    -- and cases where the name didn't change but the hash did.
    -- So, we treat these elements as equal then detect them in a post-processing step.
    diffEq :: AT.Segment Syntax.Element -> AT.Segment Syntax.Element -> Bool
    diffEq (AT.Segment {segment = fromSegment, annotation = fromAnnotation}) (AT.Segment {segment = toSegment, annotation = toAnnotation}) =
      fromSegment == toSegment || fromAnnotation == toAnnotation

    expandSpecialCases :: [Diff.Diff [AT.Segment (Syntax.Element)]] -> [SemanticSyntaxDiff]
    expandSpecialCases xs =
      xs
        & foldMap \case
          Diff.First ys -> [Old ys]
          Diff.Second ys -> [New ys]
          Diff.Both from to ->
            -- Each list should always be the same length.
            zipWith detectSpecialCase from to
              & (flip List.foldr [])
                ( \next acc -> case (acc, next) of
                    (Both xs : rest, Left seg) -> Both (seg : xs) : rest
                    (_, Left seg) -> Both [seg] : acc
                    (_, Right diff) -> diff : acc
                )
    detectSpecialCase :: AT.Segment Syntax.Element -> AT.Segment Syntax.Element -> Either (AT.Segment Syntax.Element) SemanticSyntaxDiff
    detectSpecialCase fromSegment toSegment
      | fromSegment == toSegment = Left fromSegment
      | AT.annotation fromSegment == AT.annotation toSegment = Right (SegmentChange (AT.segment fromSegment, AT.segment toSegment) (AT.annotation fromSegment))
      | AT.segment fromSegment == AT.segment toSegment = Right (AnnotationChange (AT.segment fromSegment) (AT.annotation fromSegment, AT.annotation toSegment))
      | otherwise = error "diffSyntaxText: found Syntax Elements in 'both' which have nothing in common."
