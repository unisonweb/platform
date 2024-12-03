module Unison.Runtime.Builtin.Types
  ( typeReferences,
    builtinTypeNumbering,
    builtinTypeBackref,
  )
where

import Data.Map qualified as Map
import Unison.Builtin qualified as Ty (builtinTypes)
import Unison.Builtin.Decls qualified as Ty
import Unison.Prelude hiding (Text, some)
import Unison.Reference
import Unison.Util.EnumContainers as EC

builtinTypeNumbering :: Map Reference Word64
builtinTypeNumbering = Map.fromList typeReferences

typeReferences :: [(Reference, Word64)]
typeReferences = zip rs [1 ..]
  where
    rs =
      [r | (_, r) <- Ty.builtinTypes]
        ++ [DerivedId i | (_, i, _) <- Ty.builtinDataDecls]
        ++ [DerivedId i | (_, i, _) <- Ty.builtinEffectDecls]

builtinTypeBackref :: EnumMap Word64 Reference
builtinTypeBackref = mapFromList $ swap <$> typeReferences
  where
    swap (x, y) = (y, x)
