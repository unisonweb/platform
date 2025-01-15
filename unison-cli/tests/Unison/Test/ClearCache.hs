{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Test.ClearCache where

import Data.Foldable (for_)
import Data.List.Extra (enumerate)
import Data.String.Here (i)
import EasyTest
import Unison.Codebase qualified as Codebase
import Unison.Test.Ucm qualified as Ucm
import Unison.WatchKind qualified as WatchKind

test :: Test ()
test = scope "clearWatchCache" $
  for_ enumerate \codebaseFormat -> scope (show codebaseFormat) do
    c <- io $ Ucm.initCodebase codebaseFormat
    let listWatches = io $ Ucm.lowLevel c \c ->
          Codebase.runTransaction c (Codebase.watches WatchKind.RegularWatch)

    io $
      Ucm.runTranscript
        c
        [i|
      ``` ucm
      scratch/main> alias.term ##Nat.+ +
      ```
      ``` unison
      > 1 + 1
      ```
    |]

    expectNotEqual [] =<< listWatches

    io $
      Ucm.runTranscript
        c
        [i|
      ``` ucm
      scratch/main> debug.clear-cache
      ```
    |]

    expectEqual [] =<< listWatches
