{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.Branch.Type
  ( NamespaceHash,
    head,
    headHash,
    namespaceHash,
    Branch (..),
    Branch0 (..),
    history,
    edits,
    Star,
    UnwrappedBranch,
  )
where

import Control.Lens
import Data.Map (Map)
import Data.Set (Set)
import U.Codebase.HashTags (CausalHash, PatchHash)
import Unison.Codebase.Causal.Type (Causal)
import Unison.Codebase.Causal.Type qualified as Causal
import Unison.Codebase.Metadata qualified as Metadata
import Unison.Codebase.Patch (Patch)
import Unison.Codebase.Path (Path)
import Unison.Hash qualified as Hash
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Util.Relation (Relation)
import Prelude hiding (head)

-- | A node in the Unison namespace hierarchy
-- along with its history.
newtype Branch m = Branch {_history :: UnwrappedBranch m}
  deriving (Eq, Ord)

type UnwrappedBranch m = Causal m (Branch0 m)

-- | A Hash for a namespace itself, it doesn't incorporate any history.
type NamespaceHash m = Hash.HashFor (Branch0 m)

type Star r n = Metadata.Star r n

head :: Branch m -> Branch0 m
head (Branch c) = Causal.head c

headHash :: Branch m -> CausalHash
headHash (Branch c) = Causal.currentHash c

namespaceHash :: Branch m -> NamespaceHash m
namespaceHash (Branch c) = Causal.valueHash c

-- | A node in the Unison namespace hierarchy.
--
-- '_terms' and '_types' are the declarations at this level.
-- '_children' are the nodes one level below us.
-- '_edits' are the 'Patch's stored at this node in the code.
--
-- The remaining fields are derived from the four above.
-- Please don't set them manually; use Branch.empty0 or Branch.branch0 to construct them.
data Branch0 m = Branch0
  { _terms :: Star Referent NameSegment,
    _types :: Star Reference NameSegment,
    -- | Note the 'Branch' here, not 'Branch0'.
    -- Every level in the tree has a history.
    _children :: Map NameSegment (Branch m),
    _edits :: Map NameSegment (PatchHash, m Patch),
    -- | True if a branch and its children have no definitions or edits in them.
    -- (Computed recursively, and small enough to justify storing here to avoid computing more than once.)
    isEmpty0 :: Bool,
    -- names and metadata for this branch and its children
    -- (ref, (name, value)) iff ref has metadata `value` at name `name`
    deepTerms :: Relation Referent Name,
    deepTypes :: Relation Reference Name,
    deepTermMetadata :: Metadata.R4 Referent Name,
    deepTypeMetadata :: Metadata.R4 Reference Name,
    deepPaths :: Set Path,
    deepEdits :: Map Name PatchHash
  }

instance Eq (Branch0 m) where
  a == b =
    _terms a == _terms b
      && _types a == _types b
      && _children a == _children b
      && (fmap fst . _edits) a == (fmap fst . _edits) b

history :: Iso' (Branch m) (UnwrappedBranch m)
history = iso _history Branch

edits :: Lens' (Branch0 m) (Map NameSegment (PatchHash, m Patch))
edits = lens _edits (\b0 e -> b0 {_edits = e})
