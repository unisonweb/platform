module Unison.Server.NameSearch
  ( Search (..),
    NameSearch (..),
    applySearch,
    SearchType (..),
  )
where

import Control.Lens
import Data.List qualified as List
import Data.Set qualified as Set
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.NamesWithHistory (SearchType (..))
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Server.SearchResult qualified as SR

-- | A @Search r@ is a small bag of functions that is used to power a search for @r@s.
--
-- There are currently two implementations of this interface:
--
-- * 'NameSearch.FromNames' which builds a name search from a 'Names' object
-- * 'NameSearch.Sqlite which builds a name search that uses a sqlite name lookup index.
--
-- You can use the individual methods of a name search or can use 'applySearch'.
data Search m r = Search
  { lookupNames :: r -> m (Set (HQ'.HashQualified Name)),
    lookupRelativeHQRefs' :: SearchType -> HQ'.HashQualified Name -> m (Set r),
    makeResult :: HQ.HashQualified Name -> r -> Set (HQ'.HashQualified Name) -> m SR.SearchResult,
    matchesNamedRef :: Name -> r -> HQ'.HashQualified Name -> Bool
  }

data NameSearch m = NameSearch
  { typeSearch :: Search m Reference,
    termSearch :: Search m Referent
  }

-- | Interpret a 'Search' as a function from name to search results.
applySearch :: (Show r, Monad m) => Search m r -> SearchType -> HQ'.HashQualified Name -> m [SR.SearchResult]
applySearch Search {lookupNames, lookupRelativeHQRefs', makeResult, matchesNamedRef} searchType query = do
  refs <- (lookupRelativeHQRefs' searchType query)
  -- a bunch of references will match a HQ ref.
  for (toList refs) \ref -> do
    let -- Precondition: the input set is non-empty
        prioritize :: Set (HQ'.HashQualified Name) -> (HQ'.HashQualified Name, Set (HQ'.HashQualified Name))
        prioritize =
          Set.toList
            >>> sortOn (\n -> matchesNamedRef (HQ'.toName n) ref query)
            >>> List.uncons
            >>> fromMaybe (error (reportBug "E839404" ("query = " ++ show query ++ ", ref = " ++ show ref)))
            >>> over _2 Set.fromList
    names <- lookupNames ref
    let (primaryName, aliases) =
          -- The precondition of `prioritize` should hold here because we are passing in the set of names that are
          -- related to this ref, which is itself one of the refs that the query name was related to! (Hence it should
          -- be non-empty).
          prioritize names
    makeResult (HQ'.toHQ primaryName) ref aliases
