module Unison.PrettyPrintEnvDecl.Names
  ( makePPED,
  )
where

import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (PrettyPrintEnvDecl))

makePPED :: PPE.Namer -> PPE.Suffixifier -> PrettyPrintEnvDecl
makePPED namer suffixifier =
  PrettyPrintEnvDecl
    (PPE.makePPE namer PPE.dontSuffixify)
    (PPE.makePPE namer suffixifier)
