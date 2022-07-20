{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (..), biasTo) where

import Unison.Name (Name)
import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import qualified Unison.PrettyPrintEnv as PPE

-- A pair of PrettyPrintEnvs:
--   - suffixifiedPPE uses the shortest unique suffix
--   - unsuffixifiedPPE uses the shortest full name
--
-- Generally, we want declarations LHS (the `x` in `x = 23`) to use the
-- unsuffixified names, so the LHS is an accurate description of where in the
-- namespace the definition lives. For everywhere else, we can use the
-- suffixified version.
data PrettyPrintEnvDecl = PrettyPrintEnvDecl
  { unsuffixifiedPPE :: PrettyPrintEnv,
    suffixifiedPPE :: PrettyPrintEnv
  }
  deriving (Show)

-- | Lifts 'biasTo' over a PrettyPrintEnvDecl
biasTo :: [Name] -> PrettyPrintEnvDecl -> PrettyPrintEnvDecl
biasTo targets PrettyPrintEnvDecl {unsuffixifiedPPE, suffixifiedPPE} =
  PrettyPrintEnvDecl
    { unsuffixifiedPPE = PPE.biasTo targets unsuffixifiedPPE,
      suffixifiedPPE = PPE.biasTo targets suffixifiedPPE
    }
