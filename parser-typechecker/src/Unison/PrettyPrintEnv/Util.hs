{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnv.Util (declarationPPE, declarationPPEDecl) where

import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (suffixifiedPPE, unsuffixifiedPPE))
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent

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
      | hash r == rootH = terms (unsuffixifiedPPE ppe) r0
      | otherwise = terms (suffixifiedPPE ppe) r0
    tm r = terms (suffixifiedPPE ppe) r
    ty r
      | hash r == rootH = types (unsuffixifiedPPE ppe) r
      | otherwise = types (suffixifiedPPE ppe) r

-- The suffixed names uses the fully-qualified name for `r`
declarationPPEDecl :: PrettyPrintEnvDecl -> Reference -> PrettyPrintEnvDecl
declarationPPEDecl ppe r =
  ppe {suffixifiedPPE = declarationPPE ppe r}
