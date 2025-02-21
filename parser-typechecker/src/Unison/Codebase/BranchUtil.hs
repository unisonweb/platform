module Unison.Codebase.BranchUtil
  ( -- * Branch creation
    fromNames,

    -- * Branch queries
    getBranch,
    getTerm,
    getType,

    -- * Branch modifications
    makeSetBranch,
    makeAddTypeName,
    makeDeleteTypeName,
    makeAnnihilateTypeName,
    makeAddTermName,
    makeDeleteTermName,
    makeAnnihilateTermName,
  )
where

import Control.Lens
import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.Codebase.Branch (Branch, Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.ShortHash qualified as SH
import Unison.Util.Relation qualified as R
import Unison.Util.Star2 qualified as Star2

-- | Creates a branch containing all of the given names, with a single history node.
fromNames :: (Monad m) => Names -> Branch m
fromNames names0 = Branch.stepManyAt (typeActions <> termActions) Branch.empty
  where
    typeActions = map doType . R.toList $ Names.types names0
    termActions = map doTerm . R.toList $ Names.terms names0
    doTerm (n, r) = makeAddTermName (Path.splitFromName n) r
    doType (n, r) = makeAddTypeName (Path.splitFromName n) r

getTerm :: HQ'.HashQualified (Path.Split Path) -> Branch0 m -> Set Referent
getTerm hq b = case hq of
  HQ'.NameOnly (p, n) -> Star2.lookupD1 n $ terms p
  HQ'.HashQualified (p, n) sh -> filter sh . Star2.lookupD1 n $ terms p
  where
    filter sh = Set.filter (SH.isPrefixOf sh . Referent.toShortHash)
    terms p = (Branch.getAt0 p b) ^. Branch.terms

getType :: HQ'.HashQualified (Path.Split Path) -> Branch0 m -> Set Reference.TypeReference
getType hq b = case hq of
  HQ'.NameOnly (p, n) -> Star2.lookupD1 n $ types p
  HQ'.HashQualified (p, n) sh -> filter sh . Star2.lookupD1 n $ types p
  where
    filter sh = Set.filter (SH.isPrefixOf sh . Reference.toShortHash)
    types p = (Branch.getAt0 p b) ^. Branch.types

getBranch :: Path.Split Path -> Branch0 m -> Maybe (Branch m)
getBranch (p, seg) b = case Path.toList p of
  [] -> Map.lookup seg (b ^. Branch.children)
  h : p ->
    (Branch.head <$> Map.lookup h (b ^. Branch.children))
      >>= getBranch (Path.fromList p, seg)

makeAddTermName :: Path.Split p -> Referent -> (p, Branch0 m -> Branch0 m)
makeAddTermName (p, name) r = (p, Branch.addTermName r name)

makeDeleteTermName :: Path.Split p -> Referent -> (p, Branch0 m -> Branch0 m)
makeDeleteTermName (p, name) r = (p, Branch.deleteTermName r name)

makeAnnihilateTermName :: Path.Split path -> (path, Branch0 m -> Branch0 m)
makeAnnihilateTermName (p, name) = (p, Branch.annihilateTermName name)

makeAnnihilateTypeName :: Path.Split path -> (path, Branch0 m -> Branch0 m)
makeAnnihilateTypeName (p, name) = (p, Branch.annihilateTypeName name)

makeAddTypeName :: Path.Split p -> Reference -> (p, Branch0 m -> Branch0 m)
makeAddTypeName (p, name) r = (p, Branch.addTypeName r name)

makeDeleteTypeName :: Path.Split p -> Reference -> (p, Branch0 m -> Branch0 m)
makeDeleteTypeName (p, name) r = (p, Branch.deleteTypeName r name)

makeSetBranch :: Path.Split path -> Branch m -> (path, Branch0 m -> Branch0 m)
makeSetBranch (p, name) b = (p, Branch.setChildBranch name b)
