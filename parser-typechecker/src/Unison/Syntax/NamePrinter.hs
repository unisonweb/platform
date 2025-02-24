module Unison.Syntax.NamePrinter where

import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as List.Nonempty
import Data.Text qualified as Text
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.NameSegment.Internal (NameSegment(NameSegment))
import Data.List.NonEmpty (NonEmpty(..))
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as SH
import Unison.Syntax.HashQualified qualified as HQ (toText)
import Unison.Syntax.Name qualified as Name (toText)
import Unison.Util.Pretty (Pretty)
import Unison.Util.Pretty qualified as PP
import Unison.Util.SyntaxText qualified as S

type SyntaxText = S.SyntaxText' Reference

prettyName :: (IsString s) => Name -> Pretty s
prettyName = PP.text . Name.toText

prettyHashQualified :: HQ.HashQualified Name -> Pretty SyntaxText
prettyHashQualified hq = styleHashQualified' id (fmt $ S.HashQualifier hq) hq

-- | Given (full, suffixified), render a full hash-qualified name, but with a grayed-out prefix that doesn't contribute
-- to the uniqueness of the suffix.
--
-- For example, if name "foo.bar.baz" has unique suffix "baz", then "foo.bar." will be grayed out.
prettyHashQualifiedFull :: (HQ.HashQualified Name, HQ.HashQualified Name) -> Pretty PP.ColorText
prettyHashQualifiedFull (hqFull, hqSuffixified) =
  case (HQ.toName hqFull, HQ.toName hqSuffixified) of
    (Just full, Just suffixified)
      | Just prefix <- prefixify full suffixified ->
          PP.hiBlack (PP.syntaxToColor (prettyHashQualified (HQ.NameOnly prefix)) <> ".")
            <> PP.syntaxToColor (prettyHashQualified hqSuffixified)
    _ -> PP.syntaxToColor (prettyHashQualified hqFull)
  where
    prefixify :: Name -> Name -> Maybe Name
    prefixify full suffixified =
      go (f full) (f suffixified)
      where
        f :: Name -> [NameSegment]
        f = Foldable.toList . Name.reverseSegments

        -- go ["baz", "bar", "foo"] ["baz"] = Just "foo.bar"
        go :: [NameSegment] -> [NameSegment] -> Maybe Name
        go xs [] = Name.fromReverseSegments <$> List.Nonempty.nonEmpty xs
        go (_ : xs) (_ : ys) = go xs ys
        -- Impossible, but eh, just return Nothing (could call bug instead)
        go _ _ = Nothing

prettyHashQualified' :: HQ'.HashQualified Name -> Pretty SyntaxText
prettyHashQualified' = prettyHashQualified . HQ'.toHQ

prettyHashQualified0 :: (IsString s) => HQ.HashQualified Name -> Pretty s
prettyHashQualified0 = PP.text . HQ.toText

-- | Pretty-print a reference as a name and the given number of characters of
-- its hash.
prettyNamedReference :: Int -> Name -> Reference -> Pretty SyntaxText
prettyNamedReference len name =
  prettyHashQualified . HQ.take len . HQ.fromNamedReference name

-- | Pretty-print a referent as a name and the given number of characters of its
-- hash.
prettyNamedReferent :: Int -> Name -> Referent -> Pretty SyntaxText
prettyNamedReferent len name =
  prettyHashQualified . HQ.take len . HQ.fromNamedReferent name

-- | Pretty-print a reference as the given number of characters of its hash.
prettyReference :: Int -> Reference -> Pretty SyntaxText
prettyReference len =
  prettyHashQualified . HQ.take len . HQ.fromReference

-- | Pretty-print a referent as the given number of characters of its hash.
prettyReferent :: Int -> Referent -> Pretty SyntaxText
prettyReferent len =
  prettyHashQualified . HQ.take len . HQ.fromReferent

prettyLabeledDependency :: Int -> LabeledDependency -> Pretty SyntaxText
prettyLabeledDependency len = LD.fold (prettyReference len) (prettyReferent len)

prettyShortHash :: (IsString s) => ShortHash -> Pretty s
prettyShortHash = fromString . Text.unpack . SH.toText

styleHashQualified ::
  (IsString s) => (Pretty s -> Pretty s) -> HQ.HashQualified Name -> Pretty s
styleHashQualified style hq = styleHashQualified' style id hq

styleHashQualified' ::
  (IsString s) =>
  (Pretty s -> Pretty s) ->
  (Pretty s -> Pretty s) ->
  HQ.HashQualified Name ->
  Pretty s
styleHashQualified' nameStyle hashStyle = \case
  HQ.NameOnly n -> nameStyle (prettyName n)
  HQ.HashOnly h -> hashStyle (prettyShortHash h)
  HQ.HashQualified n h ->
    PP.group $ nameStyle (prettyName n) <> hashStyle (prettyShortHash h)

styleHashQualified'' ::
  (Pretty SyntaxText -> Pretty SyntaxText) ->
  HQ.HashQualified Name ->
  Pretty SyntaxText
styleHashQualified'' nameStyle hq =
  styleHashQualified' nameStyle (fmt $ S.HashQualifier hq) hq

fmt :: S.Element r -> Pretty (S.SyntaxText' r) -> Pretty (S.SyntaxText' r)
fmt = PP.withSyntax
