module U.Codebase.Sqlite.V2.HashHandle
  ( v2HashHandle,
  )
where

import Data.Function ((&))
import Data.Set qualified as Set
import U.Codebase.Branch.Hashing qualified as H2
import U.Codebase.Causal.Hashing qualified as H2
import U.Codebase.HashTags (BranchHash (..))
import U.Codebase.Sqlite.Branch.Format qualified as BranchFormat
import U.Codebase.Sqlite.HashHandle
import U.Codebase.Term.Hashing as H2
import U.Util.Type (removeAllEffectVars)
import Unison.Hash32 qualified as Hash32
import Unison.Hashing.V2 qualified as H2
import Unison.Hashing.V2.Convert2 (h2ToV2Reference, hashBranchFormatToH2Branch, v2ToH2Type, v2ToH2TypeD)

v2HashHandle :: HashHandle
v2HashHandle =
  HashHandle
    { toReference = h2ToV2Reference . H2.typeToReference . v2ToH2Type . removeAllEffectVars,
      toReferenceMentions = Set.map h2ToV2Reference . H2.typeToReferenceMentions . v2ToH2Type . removeAllEffectVars,
      toReferenceDecl = \h -> h2ToV2Reference . H2.typeToReference . v2ToH2TypeD h . removeAllEffectVars,
      toReferenceDeclMentions = \h -> Set.map h2ToV2Reference . H2.typeToReferenceMentions . v2ToH2TypeD h . removeAllEffectVars,
      hashBranch = H2.hashBranch,
      hashBranchV3 = H2.hashBranchV3,
      hashCausal = H2.hashCausal,
      hashBranchFormatFull = \localIds localBranch ->
        BranchFormat.localToHashBranch localIds localBranch
          & hashBranchFormatToH2Branch
          & H2.contentHash
          & Hash32.fromHash
          & BranchHash,
      verifyTermFormatHash = H2.verifyTermFormatHash
    }
