-- | Utilities related to resolving names to things.
module Unison.Cli.NameResolutionUtils
  ( resolveHQName,
    resolveHQToLabeledDependencies,
  )
where

import Control.Monad.Reader (ask)
import Data.Bifoldable (bifoldMap)
import Data.Set qualified as Set
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.HashQualified qualified as HQ
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import Unison.Server.NameSearch.Sqlite qualified as Sqlite
import Unison.ShortHash (ShortHash)
import Unison.Util.Defns (Defns (..), DefnsF)

resolveHQName :: HQ.HashQualified Name -> Cli (DefnsF Set Referent TypeReference)
resolveHQName = \case
  HQ.NameOnly name -> do
    names <- Cli.currentNames
    pure
      Defns
        { terms = Name.searchByRankedSuffix name names.terms,
          types = Name.searchByRankedSuffix name names.types
        }
  -- rationale: the hash should be unique enough that the name never helps
  -- mitchell says: that seems wrong
  HQ.HashQualified _n hash -> resolveHashOnly hash
  HQ.HashOnly hash -> resolveHashOnly hash
  where
    resolveHashOnly :: ShortHash -> Cli (DefnsF Set Referent TypeReference)
    resolveHashOnly hash = do
      env <- ask
      Cli.runTransaction do
        terms <- Sqlite.termReferentsByShortHash env.codebase hash
        types <- Sqlite.typeReferencesByShortHash hash
        pure Defns {terms, types}

resolveHQToLabeledDependencies :: HQ.HashQualified Name -> Cli (Set LabeledDependency)
resolveHQToLabeledDependencies =
  fmap (bifoldMap (Set.map LD.referent) (Set.map LD.typeRef)) . resolveHQName
