module Unison.PrettyPrintEnv.Names
  ( -- * Namer
    Namer (..),
    hqNamer,
    namer,

    -- * Suffixifier
    Suffixifier,
    dontSuffixify,
    suffixifyByHash,
    suffixifyByHashName,
    suffixifyByName,
    suffixifyByHashWithUnhashedTermsInScope,

    -- * Pretty-print env
    makePPE,
    makeTermNames,
    makeTypeNames,
  )
where

import Data.Set qualified as Set
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Names.ResolvesTo (ResolvesTo (..))
import Unison.NamesWithHistory qualified as Names
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv (PrettyPrintEnv))
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation

------------------------------------------------------------------------------------------------------------------------
-- Namer

-- | A "namer" associates a set of (possibly hash-qualified) names with a referent / type reference.
data Namer = Namer
  { nameTerm :: Referent -> Set (HQ'.HashQualified Name),
    nameType :: TypeReference -> Set (HQ'.HashQualified Name)
  }

-- | Make a "namer" out of a collection of names, ignoring conflicted names. That is, if references #foo and #bar are
-- both associated with name "baz", then the returned namer maps #foo too "baz" (not "baz"#foo) and #bar to "baz" (not
-- "baz"#bar).
namer :: Names -> Namer
namer names =
  Namer
    { nameTerm = Set.map HQ'.fromName . Names.namesForReferent names,
      nameType = Set.map HQ'.fromName . Names.namesForReference names
    }

-- | Make a "namer" out of a collection of names, respecting conflicted names. That is, if references #foo and #bar are
-- both associated with name "baz", then the returned namer maps #foo too "baz"#foo and #bar to "baz"#bar, but otherwise
-- if a reference #qux has a single name "qux", then the returned namer maps #qux to "qux" (not "qux"#qux).
hqNamer :: Int -> Names -> Namer
hqNamer hashLen names =
  Namer
    { nameTerm = \ref -> Names.termName hashLen ref names,
      nameType = \ref -> Names.typeName hashLen ref names
    }

------------------------------------------------------------------------------------------------------------------------
-- Suffixifier

data Suffixifier = Suffixifier
  { suffixifyTerm :: Name -> Name,
    suffixifyType :: Name -> Name
  }

dontSuffixify :: Suffixifier
dontSuffixify =
  Suffixifier id id

suffixifyByName :: Names -> Suffixifier
suffixifyByName names =
  Suffixifier
    { suffixifyTerm = \name -> Name.suffixifyByName name (Names.terms names),
      suffixifyType = \name -> Name.suffixifyByName name (Names.types names)
    }

suffixifyByHash :: Names -> Suffixifier
suffixifyByHash names =
  Suffixifier
    { suffixifyTerm = \name -> Name.suffixifyByHash name (Names.terms names),
      suffixifyType = \name -> Name.suffixifyByHash name (Names.types names)
    }

suffixifyByHashName :: Names -> Suffixifier
suffixifyByHashName names =
  Suffixifier
    { suffixifyTerm = \name -> Name.suffixifyByHashName name (Names.terms names),
      suffixifyType = \name -> Name.suffixifyByHashName name (Names.types names)
    }

suffixifyByHashWithUnhashedTermsInScope :: Set Name -> Names -> Suffixifier
suffixifyByHashWithUnhashedTermsInScope localTermNames namespaceNames =
  Suffixifier
    { suffixifyTerm = \name -> Name.suffixifyByHash name terms,
      suffixifyType = \name -> Name.suffixifyByHash name (Names.types namespaceNames)
    }
  where
    terms :: Relation Name (ResolvesTo Referent)
    terms =
      Names.terms namespaceNames
        & Relation.subtractDom localTermNames
        & Relation.mapRan ResolvesToNamespace
        & Relation.union (Relation.fromList (map (\name -> (name, ResolvesToLocal name)) (Set.toList localTermNames)))

------------------------------------------------------------------------------------------------------------------------
-- Pretty-print env

makePPE :: Namer -> Suffixifier -> PrettyPrintEnv
makePPE namer suffixifier =
  PrettyPrintEnv
    (makeTermNames namer suffixifier)
    (makeTypeNames namer suffixifier)

makeTermNames :: Namer -> Suffixifier -> Referent -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
makeTermNames Namer {nameTerm} Suffixifier {suffixifyTerm} =
  prioritize . map (\name -> (name, suffixifyTerm <$> name)) . Set.toList . nameTerm

makeTypeNames :: Namer -> Suffixifier -> TypeReference -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
makeTypeNames Namer {nameType} Suffixifier {suffixifyType} =
  prioritize . map (\name -> (name, suffixifyType <$> name)) . Set.toList . nameType

-- | Sort the names for a given ref by the following factors (in priority order):
--
-- 1. Prefer Relative Names to Absolute Names
-- 2. Prefer names that aren't hash qualified to those that are
-- 3. Prefer names which have fewer segments in their fully-qualified form
-- 4. Prefer names which have fewer segments in their suffixified form (if applicable)
prioritize :: [(HQ'.HashQualified Name, HQ'.HashQualified Name)] -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
prioritize =
  sortOn \case
    (fqn, HQ'.NameOnly name) -> (Name.isAbsolute name, Nothing, Name.countSegments (HQ'.toName fqn), Name.countSegments name)
    (fqn, HQ'.HashQualified name hash) -> (Name.isAbsolute name, Just hash, Name.countSegments (HQ'.toName fqn), Name.countSegments name)
