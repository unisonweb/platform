module Unison.Merge
  ( Mergeblob0 (..),
    makeMergeblob0,
    Mergeblob1 (..),
    makeMergeblob1,
    Mergeblob2 (..),
    Mergeblob2Error (..),
    makeMergeblob2,
    Mergeblob3 (..),
    makeMergeblob3,
    Mergeblob4 (..),
    makeMergeblob4,
    Mergeblob5 (..),
    makeMergeblob5,

    -- * Decl coherency checks
    PartialDeclNameLookup (..),
    IncoherentDeclReason (..),
    checkDeclCoherency,
    lenientCheckDeclCoherency,
    IncoherentDeclReasons (..),
    checkAllDeclCoherency,

    -- * Types
    CombinedDiffOp (..),
    DiffOp (..),
    EitherWay (..),
    EitherWayI (..),
    HumanDiffOp (..),
    LibdepDiffOp (..),
    Synhashed (..),
    ThreeWay (..),
    TwoOrThreeWay (..),
    TwoWay (..),
    TwoWayI (..),
    Unconflicts (..),
    Updated (..),
  )
where

import Unison.Merge.CombineDiffs (CombinedDiffOp (..))
import Unison.Merge.DeclCoherencyCheck
  ( IncoherentDeclReason (..),
    IncoherentDeclReasons (..),
    checkAllDeclCoherency,
    checkDeclCoherency,
    lenientCheckDeclCoherency,
  )
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.EitherWay (EitherWay (..))
import Unison.Merge.EitherWayI (EitherWayI (..))
import Unison.Merge.HumanDiffOp (HumanDiffOp (..))
import Unison.Merge.Libdeps (LibdepDiffOp (..))
import Unison.Merge.Mergeblob0 (Mergeblob0 (..), makeMergeblob0)
import Unison.Merge.Mergeblob1 (Mergeblob1 (..), makeMergeblob1)
import Unison.Merge.Mergeblob2 (Mergeblob2 (..), Mergeblob2Error (..), makeMergeblob2)
import Unison.Merge.Mergeblob3 (Mergeblob3 (..), makeMergeblob3)
import Unison.Merge.Mergeblob4 (Mergeblob4 (..), makeMergeblob4)
import Unison.Merge.Mergeblob5 (Mergeblob5 (..), makeMergeblob5)
import Unison.Merge.PartialDeclNameLookup (PartialDeclNameLookup (..))
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.ThreeWay (ThreeWay (..))
import Unison.Merge.TwoOrThreeWay (TwoOrThreeWay (..))
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.TwoWayI (TwoWayI (..))
import Unison.Merge.Unconflicts (Unconflicts (..))
import Unison.Merge.Updated (Updated (..))
