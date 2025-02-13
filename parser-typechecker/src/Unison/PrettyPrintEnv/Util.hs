{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnv.Util (declarationPPE) where

import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (suffixifiedPPE, unsuffixifiedPPE))
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent

-- declarationPPE uses the full name for references that are
-- part the same cycle as the input reference, used to ensures
-- recursive definitions are printed properly, for instance:
--
-- foo.bar x = foo.bar x
-- and not
-- foo.bar x = bar x
declarationPPE :: PrettyPrintEnvDecl -> Reference -> PrettyPrintEnv
declarationPPE ppe ref = PrettyPrintEnv tm ty
  where
    rootH = hash ref
    hash Reference.Builtin {} = Nothing
    hash (Reference.Derived h _) = Just h
    tm r0@(Referent.Ref r)
      | hash r == rootH = PPE.termNames (unsuffixifiedPPE ppe) r0
      | otherwise = PPE.termNames (suffixifiedPPE ppe) r0
    tm r = PPE.termNames (suffixifiedPPE ppe) r
    ty r
      | hash r == rootH = PPE.typeNames (unsuffixifiedPPE ppe) r
      | otherwise = PPE.typeNames (suffixifiedPPE ppe) r
