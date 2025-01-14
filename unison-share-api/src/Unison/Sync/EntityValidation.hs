{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Module for validating hashes of entities received/sent via sync.
module Unison.Sync.EntityValidation
  ( validateEntity,
    validateTempEntity,
  )
where

import Data.ByteString qualified as BS
import Data.Bytes.Get (runGetS)
import Data.Set qualified as Set
import Data.Text qualified as Text
import U.Codebase.HashTags
import U.Codebase.Sqlite.Branch.Format qualified as BranchFormat
import U.Codebase.Sqlite.Causal qualified as CausalFormat
import U.Codebase.Sqlite.Decl.Format qualified as DeclFormat
import U.Codebase.Sqlite.Decode qualified as Decode
import U.Codebase.Sqlite.Entity qualified as Entity
import U.Codebase.Sqlite.HashHandle qualified as HH
import U.Codebase.Sqlite.Orphans ()
import U.Codebase.Sqlite.Patch.Format qualified as PatchFormat
import U.Codebase.Sqlite.Serialization qualified as Serialization
import U.Codebase.Sqlite.TempEntity (TempEntity)
import U.Codebase.Sqlite.Term.Format qualified as TermFormat
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import Unison.Hash (Hash)
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32
import Unison.Hashing.V2 qualified as H
import Unison.Prelude
import Unison.Sync.Common qualified as Share
import Unison.Sync.Types qualified as Share

-- | Note: We currently only validate Namespace hashes.
-- We should add more validation as more entities are shared.
validateEntity :: Hash32 -> Share.Entity Text Hash32 Hash32 -> Maybe Share.EntityValidationError
validateEntity expectedHash32 entity = do
  validateTempEntity expectedHash32 $ Share.entityToTempEntity id entity

-- | Note: We currently only validate Namespace hashes.
-- We should add more validation as more entities are shared.
validateTempEntity :: Hash32 -> TempEntity -> Maybe Share.EntityValidationError
validateTempEntity expectedHash32 tempEntity = do
  case tempEntity of
    Entity.TC (TermFormat.SyncTerm localComp) -> do
      validateTerm expectedHash localComp
    Entity.DC (DeclFormat.SyncDecl localComp) -> do
      validateDecl expectedHash localComp
    Entity.N (BranchFormat.SyncDiff {}) -> do
      Just $ Share.UnsupportedEntityType expectedHash32 Share.NamespaceDiffType
    Entity.N (BranchFormat.SyncFull localIds (BranchFormat.LocalBranchBytes bytes)) -> do
      validateBranchFull expectedHash localIds bytes
    Entity.C CausalFormat.SyncCausalFormat {valueHash, parents} -> do
      validateCausal expectedHash32 valueHash (toList parents)
    Entity.P (PatchFormat.SyncDiff {}) -> do
      Just $ Share.UnsupportedEntityType expectedHash32 Share.PatchDiffType
    Entity.P (PatchFormat.SyncFull localIds bytes) -> do
      validatePatchFull expectedHash32 localIds bytes
  where
    expectedHash :: Hash
    expectedHash = Hash32.toHash expectedHash32

validatePatchFull :: Hash32 -> PatchFormat.PatchLocalIds' Text Hash32 Hash32 -> BS.ByteString -> Maybe Share.EntityValidationError
validatePatchFull expectedHash32 localIds bytes = do
  let expectedHash = Hash32.toHash expectedHash32
  case runGetS Serialization.getLocalPatch bytes of
    Left e -> Just $ Share.InvalidByteEncoding expectedHash32 Share.PatchType (Text.pack e)
    Right localPatch -> do
      let localIds' =
            localIds
              { PatchFormat.patchTextLookup = PatchFormat.patchTextLookup localIds,
                PatchFormat.patchHashLookup = ComponentHash . Hash32.toHash <$> PatchFormat.patchHashLookup localIds,
                PatchFormat.patchDefnLookup = ComponentHash . Hash32.toHash <$> PatchFormat.patchDefnLookup localIds
              }
      let actualHash =
            HH.hashPatchFormatFull v2HashHandle localIds' localPatch
      if actualHash == PatchHash expectedHash
        then Nothing
        else Just $ Share.EntityHashMismatch Share.PatchType (mismatch expectedHash (unPatchHash actualHash))

validateBranchFull ::
  Hash ->
  BranchFormat.BranchLocalIds' Text Hash32 Hash32 (Hash32, Hash32) ->
  BS.ByteString ->
  (Maybe Share.EntityValidationError)
validateBranchFull expectedHash localIds bytes = do
  case runGetS Serialization.getLocalBranch bytes of
    Left e -> Just $ Share.InvalidByteEncoding (Hash32.fromHash expectedHash) Share.NamespaceType (Text.pack e)
    Right localBranch -> do
      let localIds' =
            localIds
              { BranchFormat.branchDefnLookup = ComponentHash . Hash32.toHash <$> BranchFormat.branchDefnLookup localIds,
                BranchFormat.branchPatchLookup = PatchHash . Hash32.toHash <$> BranchFormat.branchPatchLookup localIds,
                BranchFormat.branchChildLookup =
                  BranchFormat.branchChildLookup localIds
                    <&> bimap (BranchHash . Hash32.toHash) (CausalHash . Hash32.toHash)
              }
      let actualHash =
            HH.hashBranchFormatFull v2HashHandle localIds' localBranch
      if actualHash == BranchHash expectedHash
        then Nothing
        else Just $ Share.EntityHashMismatch Share.NamespaceType (mismatch expectedHash (unBranchHash actualHash))

validateTerm :: Hash -> (TermFormat.SyncLocallyIndexedComponent' Text Hash32) -> (Maybe Share.EntityValidationError)
validateTerm expectedHash syncLocalComp = do
  case Decode.unsyncTermComponent syncLocalComp of
    Left decodeErr -> Just (Share.InvalidByteEncoding (Hash32.fromHash expectedHash) Share.TermComponentType (tShow decodeErr))
    Right localComp -> do
      case HH.verifyTermFormatHash v2HashHandle (ComponentHash expectedHash) (TermFormat.Term localComp) of
        Nothing -> Nothing
        Just (HH.HashMismatch {expectedHash, actualHash}) -> Just . Share.EntityHashMismatch Share.TermComponentType $ mismatch expectedHash actualHash

validateDecl :: Hash -> (DeclFormat.SyncLocallyIndexedComponent' Text Hash32) -> (Maybe Share.EntityValidationError)
validateDecl expectedHash syncLocalComp = do
  case Decode.unsyncDeclComponent syncLocalComp of
    Left decodeErr -> Just (Share.InvalidByteEncoding (Hash32.fromHash expectedHash) Share.DeclComponentType (tShow decodeErr))
    Right localComp -> do
      case HH.verifyDeclFormatHash v2HashHandle (ComponentHash expectedHash) (DeclFormat.Decl localComp) of
        Nothing -> Nothing
        Just (HH.DeclHashMismatch (HH.HashMismatch {expectedHash, actualHash})) -> Just . Share.EntityHashMismatch Share.DeclComponentType $ mismatch expectedHash actualHash
        Just HH.DeclHashResolutionFailure -> Just $ Share.HashResolutionFailure (Hash32.fromHash expectedHash)

validateCausal :: Hash32 -> Hash32 -> [Hash32] -> Maybe Share.EntityValidationError
validateCausal expectedHash32 valueHash32 parentHashes32 = do
  let expectedHash = Hash32.toHash expectedHash32
  let branchHash = Hash32.toHash valueHash32
  let parents = Set.fromList (Hash32.toHash <$> parentHashes32)
  let actualHash = H.contentHash (H.Causal {branchHash, parents})
  if actualHash == expectedHash
    then Nothing
    else Just $ Share.EntityHashMismatch Share.CausalType (mismatch expectedHash actualHash)

mismatch :: Hash -> Hash -> Share.HashMismatchForEntity
mismatch supplied computed =
  Share.HashMismatchForEntity
    { supplied = Hash32.fromHash supplied,
      computed = Hash32.fromHash computed
    }
