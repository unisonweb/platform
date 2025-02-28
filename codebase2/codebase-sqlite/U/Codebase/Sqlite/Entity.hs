module U.Codebase.Sqlite.Entity where

import U.Codebase.Sqlite.Branch.Format qualified as Namespace
import U.Codebase.Sqlite.Causal qualified as Causal
import U.Codebase.Sqlite.DbId (BranchHashId, BranchObjectId, CausalHashId, HashId, ObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.Decl.Format qualified as Decl
import U.Codebase.Sqlite.Patch.Format qualified as Patch
import U.Codebase.Sqlite.TempEntityType (TempEntityType (..))
import U.Codebase.Sqlite.Term.Format qualified as Term

-- |
-- data SyncEntity
--   = TC SyncTermFormat
--   | DC SyncDeclFormat
--   | N SyncBranchFormat
--   | P SyncPatchFormat
--   | C SyncCausalFormat
type SyncEntity =
  SyncEntity' TextId HashId ObjectId PatchObjectId BranchHashId BranchObjectId CausalHashId

data SyncEntity' text hash defn patch branchh branch causal
  = TC (Term.SyncTermFormat' text defn)
  | DC (Decl.SyncDeclFormat' text defn)
  | N (Namespace.SyncBranchFormat' branch text defn patch (branch, causal))
  | P (Patch.SyncPatchFormat' patch text hash defn)
  | C (Causal.SyncCausalFormat' causal branchh)
  deriving stock (Eq, Show)

entityType :: SyncEntity' text hash defn patch branchh branch causal -> TempEntityType
entityType = \case
  TC _ -> TermComponentType
  DC _ -> DeclComponentType
  N _ -> NamespaceType
  P _ -> PatchType
  C _ -> CausalType
