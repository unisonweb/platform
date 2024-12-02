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
    --
    -- The diff algorithm only understands whether items are equal or not, so in order to add this special behavior we
    -- treat these special cases as equal, then we can detect and expand them in a post-processing step.
    diffEq :: AT.Segment Syntax.Element -> AT.Segment Syntax.Element -> Bool
    diffEq (AT.Segment {segment = fromSegment, annotation = fromAnnotation}) (AT.Segment {segment = toSegment, annotation = toAnnotation}) =
      fromSegment == toSegment
        || case (fromAnnotation, toAnnotation) of
          (Nothing, _) -> False
          (_, Nothing) -> False
          (Just a, Just b) ->
            case a of
              -- The set of annotations we want to special-case
              Syntax.TypeReference {} -> a == b
              Syntax.TermReference {} -> a == b
              Syntax.DataConstructorReference {} -> a == b
              Syntax.AbilityConstructorReference {} -> a == b
              Syntax.HashQualifier {} -> a == b
              _ -> False

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
                    (_, Right diff) -> diff ++ acc
                )
    detectSpecialCase :: AT.Segment Syntax.Element -> AT.Segment Syntax.Element -> Either (AT.Segment Syntax.Element) [SemanticSyntaxDiff]
    detectSpecialCase fromSegment toSegment
      | fromSegment == toSegment = Left fromSegment
      | AT.annotation fromSegment == AT.annotation toSegment = Right [SegmentChange (AT.segment fromSegment, AT.segment toSegment) (AT.annotation fromSegment)]
      -- We only emit an annotation change if it's a change in just the hash of the element (optionally the KIND of hash reference can change too).
      | AT.segment fromSegment == AT.segment toSegment,
        Just _fromHash <- AT.annotation fromSegment >>= elementHash,
        Just _toHash <- AT.annotation toSegment >>= elementHash =
          Right [AnnotationChange (AT.segment fromSegment) (AT.annotation fromSegment, AT.annotation toSegment)]
      | otherwise =
          -- the annotation changed, but it's not a recognized hash change.
          -- This can happen in certain special cases, e.g. a paren changed from being a syntax element into being part
          -- of a unit.
          -- We just emit both as old/new segments.
          Right [Old [fromSegment], New [toSegment]]
      where
        elementHash :: Syntax.Element -> Maybe Syntax.UnisonHash
        elementHash = \case
          Syntax.TypeReference hash -> Just hash
          Syntax.TermReference hash -> Just hash
          Syntax.DataConstructorReference hash -> Just hash
          Syntax.AbilityConstructorReference hash -> Just hash
          _ -> Nothing
