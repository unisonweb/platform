-- | Hedghog generators for Sync types.
module Unison.Test.Sync.Gen
  ( genTempEntity,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Short qualified as BShort
import Data.Text (Text)
import Data.Vector qualified as Vector
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import U.Codebase.Sqlite.Branch.Format qualified as BranchFormat
import U.Codebase.Sqlite.Causal qualified as CausalFormat
import U.Codebase.Sqlite.Decl.Format qualified as DeclFormat
import U.Codebase.Sqlite.Entity qualified as Entity
import U.Codebase.Sqlite.LocalIds qualified as LocalIds
import U.Codebase.Sqlite.Patch.Format qualified as PatchFormat
import U.Codebase.Sqlite.TempEntity (TempEntity)
import U.Codebase.Sqlite.Term.Format qualified as TermFormat
import Unison.Hash (Hash (..))
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32

genTempEntity :: Gen TempEntity
genTempEntity = do
  Gen.choice
    [ Entity.TC <$> genSyncTermFormat,
      Entity.DC <$> genSyncDeclFormat,
      Entity.P <$> genPatchFormat,
      Entity.N <$> genNamespaceFormat,
      Entity.C <$> genCausalFormat
    ]

genSyncTermFormat :: Gen (TermFormat.SyncTermFormat' Text Hash32)
genSyncTermFormat = do
  elems <- Gen.list (Range.linear 1 4) do
    localIds <- genLocalIds
    term <- genBodyBytes
    pure (localIds, term)
  pure $ TermFormat.SyncTerm $ TermFormat.SyncLocallyIndexedComponent $ Vector.fromList elems

genSyncDeclFormat :: Gen (DeclFormat.SyncDeclFormat' Text Hash32)
genSyncDeclFormat = do
  elems <- Gen.list (Range.linear 1 4) do
    localIds <- genLocalIds
    decl <- genBodyBytes
    pure (localIds, decl)
  pure $ DeclFormat.SyncDecl $ DeclFormat.SyncLocallyIndexedComponent $ Vector.fromList elems

genPatchFormat :: Gen (PatchFormat.SyncPatchFormat' Hash32 Text Hash32 Hash32)
genPatchFormat = do
  patchTextLookup <- Vector.fromList <$> Gen.list (Range.linear 0 5) genTextLiteral
  patchHashLookup <- Vector.fromList <$> Gen.list (Range.linear 0 5) genHash32
  patchDefnLookup <- Vector.fromList <$> Gen.list (Range.linear 0 5) genHash32
  let localIds = PatchFormat.LocalIds {patchTextLookup, patchHashLookup, patchDefnLookup}
  body <- genBodyBytes
  pure $ PatchFormat.SyncFull localIds body

genNamespaceFormat :: Gen (BranchFormat.SyncBranchFormat' Hash32 Text Hash32 Hash32 (Hash32, Hash32))
genNamespaceFormat = do
  branchTextLookup <- Vector.fromList <$> Gen.list (Range.linear 0 5) genTextLiteral
  branchDefnLookup <- Vector.fromList <$> Gen.list (Range.linear 0 5) genHash32
  branchPatchLookup <- Vector.fromList <$> Gen.list (Range.linear 0 5) genHash32
  branchChildLookup <- Vector.fromList <$> Gen.list (Range.linear 0 5) ((,) <$> genHash32 <*> genHash32)
  let branchLocalIds = BranchFormat.LocalIds {branchTextLookup, branchDefnLookup, branchPatchLookup, branchChildLookup}
  body <- BranchFormat.LocalBranchBytes <$> genBodyBytes
  pure $ BranchFormat.SyncFull branchLocalIds body

genCausalFormat :: Gen (CausalFormat.SyncCausalFormat' Hash32 Hash32)
genCausalFormat = do
  valueHash <- genHash32
  parents <- Vector.fromList <$> Gen.list (Range.linear 0 5) genHash32
  pure $ CausalFormat.SyncCausalFormat {valueHash, parents}

genBodyBytes :: Gen ByteString
genBodyBytes = Gen.bytes (Range.linear 0 100)

genLocalIds :: Gen (LocalIds.LocalIds' Text Hash32)
genLocalIds = do
  textLookup <- Vector.fromList <$> Gen.list (Range.linear 0 10) genTextLiteral
  defnLookup <- Vector.fromList <$> Gen.list (Range.linear 0 10) genHash32
  pure $ LocalIds.LocalIds {textLookup, defnLookup}

genHash32 :: Gen Hash32
genHash32 = Hash32.fromHash <$> genHash

genHash :: Gen Hash
genHash = Hash . BShort.toShort <$> Gen.bytes (Range.singleton 64)

genTextLiteral :: Gen Text
genTextLiteral = Gen.text (Range.linear 0 100) Gen.unicodeAll
