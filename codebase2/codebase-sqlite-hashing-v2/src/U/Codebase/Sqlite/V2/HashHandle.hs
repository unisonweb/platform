module U.Codebase.Sqlite.V2.HashHandle
  ( v2HashHandle,
  )
where

import Data.Set qualified as Set
import U.Codebase.Branch.Hashing qualified as H2
import U.Codebase.Causal.Hashing qualified as H2
import U.Codebase.Sqlite.HashHandle
import U.Util.Type (removeAllEffectVars)
import Unison.Hashing.V2 qualified as H2
import Unison.Hashing.V2.Convert2 (h2ToV2Reference, h2ToV2ReferenceId, v2ToH2Term, v2ToH2Type, v2ToH2TypeD)

v2HashHandle :: HashHandle
v2HashHandle =
  HashHandle
    { toReference = h2ToV2Reference . H2.typeToReference . v2ToH2Type . removeAllEffectVars,
      toReferenceMentions = Set.map h2ToV2Reference . H2.typeToReferenceMentions . v2ToH2Type . removeAllEffectVars,
      toReferenceDecl = \h -> h2ToV2Reference . H2.typeToReference . v2ToH2TypeD h . removeAllEffectVars,
      toReferenceDeclMentions = \h -> Set.map h2ToV2Reference . H2.typeToReferenceMentions . v2ToH2TypeD h . removeAllEffectVars,
      hashTerm = h2ToV2ReferenceId . H2.hashClosedTerm . v2ToH2Term,
      hashBranch = H2.hashBranch,
      hashCausal = H2.hashCausal
    }
