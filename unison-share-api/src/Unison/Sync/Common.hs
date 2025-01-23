-- | Combinators or utilities shared by sync server AND client
module Unison.Sync.Common
  ( expectEntity,

    -- * Type conversions
    causalHashToHash32,
    hash32ToCausalHash,
    entityToTempEntity,
    tempEntityToEntity,
  )
where

import Control.Lens qualified as Lens
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import U.Codebase.HashTags (CausalHash (..))
import U.Codebase.Sqlite.Branch.Format qualified as NamespaceFormat
import U.Codebase.Sqlite.Causal qualified as Causal
import U.Codebase.Sqlite.Decl.Format qualified as DeclFormat
import U.Codebase.Sqlite.Entity qualified as Entity
import U.Codebase.Sqlite.LocalIds
import U.Codebase.Sqlite.Patch.Format qualified as Patch
import U.Codebase.Sqlite.Patch.Format qualified as PatchFormat
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.Term.Format qualified as TermFormat
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite
import Unison.Sync.Types qualified as Share

-- | Read an entity out of the database that we know is in main storage.
expectEntity :: Hash32 -> Sqlite.Transaction (Share.Entity Text Hash32 Hash32)
expectEntity hash = do
  syncEntity <- Q.expectEntity hash
  tempEntity <- Q.syncToTempEntity syncEntity
  pure (tempEntityToEntity tempEntity)

-- FIXME this isn't the right module  for this conversion
causalHashToHash32 :: CausalHash -> Hash32
causalHashToHash32 =
  Hash32.fromHash . unCausalHash

-- FIXME this isn't the right module  for this conversion
hash32ToCausalHash :: Hash32 -> CausalHash
hash32ToCausalHash =
  CausalHash . Hash32.toHash

-- | Convert an entity that came over the wire from Unison Share into an equivalent type that we can store in the
-- `temp_entity` table.
-- entityToTempEntity :: forall hash hash'. (hash -> hash') -> Share.Entity Text Hash32 hash -> Entity.SyncEntity' Text _ _ _ _ _ _
entityToTempEntity :: (a -> branchh) -> Share.Entity t hash a -> Entity.SyncEntity' t hash branchh branchh branchh branchh branchh
entityToTempEntity toHash32 = \case
  Share.TC (Share.TermComponent terms) ->
    terms
      & Vector.fromList
      & Vector.map (Lens.over Lens._1 mungeLocalIds)
      & TermFormat.SyncLocallyIndexedComponent
      & TermFormat.SyncTerm
      & Entity.TC
  Share.DC (Share.DeclComponent decls) ->
    decls
      & Vector.fromList
      & Vector.map (Lens.over Lens._1 mungeLocalIds)
      & DeclFormat.SyncLocallyIndexedComponent
      & DeclFormat.SyncDecl
      & Entity.DC
  Share.P Share.Patch {textLookup, oldHashLookup, newHashLookup, bytes} ->
    Entity.P (PatchFormat.SyncFull (mungePatchLocalIds textLookup oldHashLookup newHashLookup) bytes)
  Share.PD Share.PatchDiff {parent, textLookup, oldHashLookup, newHashLookup, bytes} ->
    Entity.P
      ( PatchFormat.SyncDiff
          (toHash32 parent)
          (mungePatchLocalIds textLookup oldHashLookup newHashLookup)
          bytes
      )
  Share.N Share.Namespace {textLookup, defnLookup, patchLookup, childLookup, bytes} ->
    Entity.N (NamespaceFormat.SyncFull (mungeNamespaceLocalIds textLookup defnLookup patchLookup childLookup) bytes)
  Share.ND Share.NamespaceDiff {parent, textLookup, defnLookup, patchLookup, childLookup, bytes} ->
    Entity.N
      ( NamespaceFormat.SyncDiff
          (toHash32 parent)
          (mungeNamespaceLocalIds textLookup defnLookup patchLookup childLookup)
          bytes
      )
  Share.C Share.Causal {namespaceHash, parents} ->
    Entity.C
      Causal.SyncCausalFormat
        { valueHash = toHash32 namespaceHash,
          parents = Vector.fromList (map toHash32 (Set.toList parents))
        }
  where
    -- mungeLocalIds :: Share.LocalIds Text hash -> TempEntity.TempLocalIds
    mungeLocalIds Share.LocalIds {texts, hashes} =
      LocalIds
        { textLookup = Vector.fromList texts,
          defnLookup = Vector.map toHash32 (Vector.fromList hashes)
        }

    -- mungeNamespaceLocalIds ::
    --   [Text] ->
    --   [hash] ->
    --   [hash] ->
    --   [(hash, hash)] ->
    --   TempEntity.TempNamespaceLocalIds
    mungeNamespaceLocalIds textLookup defnLookup patchLookup childLookup =
      NamespaceFormat.LocalIds
        { branchTextLookup = Vector.fromList textLookup,
          branchDefnLookup = Vector.fromList (map toHash32 defnLookup),
          branchPatchLookup = Vector.fromList (map toHash32 patchLookup),
          branchChildLookup = Vector.fromList (map (\(x, y) -> (toHash32 x, toHash32 y)) childLookup)
        }

    -- mungePatchLocalIds :: [Text] -> [_] -> [hash] -> Patch.PatchLocalIds' Text _ hash'
    mungePatchLocalIds textLookup oldHashLookup newHashLookup =
      PatchFormat.LocalIds
        { patchTextLookup = Vector.fromList textLookup,
          patchHashLookup = Vector.fromList oldHashLookup,
          patchDefnLookup = Vector.fromList (map toHash32 newHashLookup)
        }

-- tempEntityToEntity :: Sqlite.TempEntity -> Share.Entity Text Hash32 Hash32
tempEntityToEntity :: (Ord h) => Entity.SyncEntity' text oldHash h h h h h -> Share.Entity text oldHash h
tempEntityToEntity = \case
  Entity.TC (TermFormat.SyncTerm (TermFormat.SyncLocallyIndexedComponent terms)) ->
    terms
      & Vector.map (Lens.over Lens._1 mungeLocalIds)
      & Vector.toList
      & Share.TermComponent
      & Share.TC
  Entity.DC (DeclFormat.SyncDecl (DeclFormat.SyncLocallyIndexedComponent decls)) ->
    decls
      & Vector.map (Lens.over Lens._1 mungeLocalIds)
      & Vector.toList
      & Share.DeclComponent
      & Share.DC
  Entity.P format ->
    case format of
      PatchFormat.SyncFull PatchFormat.LocalIds {patchTextLookup, patchHashLookup, patchDefnLookup} bytes ->
        Share.P
          Share.Patch
            { textLookup = Vector.toList patchTextLookup,
              oldHashLookup = Vector.toList patchHashLookup,
              newHashLookup = Vector.toList patchDefnLookup,
              bytes
            }
      PatchFormat.SyncDiff parent PatchFormat.LocalIds {patchTextLookup, patchHashLookup, patchDefnLookup} bytes ->
        Share.PD
          Share.PatchDiff
            { parent,
              textLookup = Vector.toList patchTextLookup,
              oldHashLookup = Vector.toList patchHashLookup,
              newHashLookup = Vector.toList patchDefnLookup,
              bytes
            }
  Entity.N format ->
    case format of
      NamespaceFormat.SyncFull
        NamespaceFormat.LocalIds
          { branchTextLookup,
            branchDefnLookup,
            branchPatchLookup,
            branchChildLookup
          }
        bytes ->
          Share.N
            Share.Namespace
              { textLookup = Vector.toList branchTextLookup,
                defnLookup = Vector.toList branchDefnLookup,
                patchLookup = Vector.toList branchPatchLookup,
                childLookup = Vector.toList branchChildLookup,
                bytes
              }
      NamespaceFormat.SyncDiff
        parent
        NamespaceFormat.LocalIds
          { branchTextLookup,
            branchDefnLookup,
            branchPatchLookup,
            branchChildLookup
          }
        bytes ->
          Share.ND
            Share.NamespaceDiff
              { parent,
                textLookup = Vector.toList branchTextLookup,
                defnLookup = Vector.toList branchDefnLookup,
                patchLookup = Vector.toList branchPatchLookup,
                childLookup = Vector.toList branchChildLookup,
                bytes
              }
  Entity.C Causal.SyncCausalFormat {valueHash, parents} ->
    Share.C
      Share.Causal
        { namespaceHash = valueHash,
          parents = Set.fromList (Vector.toList parents)
        }
  where
    -- mungeLocalIds :: LocalIds' Text Hash32 -> Share.LocalIds Text Hash32
    mungeLocalIds LocalIds {textLookup, defnLookup} =
      Share.LocalIds
        { texts = Vector.toList textLookup,
          hashes = Vector.toList defnLookup
        }
