{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.UnisonFile.Env (Env (..), datas) where

import Unison.DataDeclaration (DataDeclaration, EffectDeclaration (..))
import Unison.Names (Names)
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference

data Env v a = Env
  -- Data declaration name to hash and its fully resolved form
  { datasId :: Map v (Reference.Id, DataDeclaration v a),
    -- Effect declaration name to hash and its fully resolved form
    effectsId :: Map v (Reference.Id, EffectDeclaration v a),
    -- Naming environment
    names :: Names
  }

datas :: Env v a -> Map v (Reference, DataDeclaration v a)
datas = fmap (first Reference.DerivedId) . datasId
