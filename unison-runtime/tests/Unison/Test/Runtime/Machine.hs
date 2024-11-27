{-# LANGUAGE TemplateHaskell #-}

module Unison.Test.Runtime.Machine () where

import Language.Haskell.TH
import Test.Inspection
import Unison.Runtime.Machine (eval)
import Unison.Runtime.Stack (Stack)

inspect $ 'eval `hasNoType` ''Stack

hasNoAllocations :: Name -> Name -> Obligation
hasNoAllocations n tn = mkObligation n (NoTypes [tn])
