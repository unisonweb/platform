module Unison.Names.ResolvesTo
  ( ResolvesTo (..),
    partitionResolutions,
  )
where

import Unison.Name (Name)
import Unison.Prelude

data ResolvesTo ref
  = ResolvesToNamespace ref
  | ResolvesToLocal Name
  deriving stock (Eq, Ord, Show)

partitionResolutions :: [(v, ResolvesTo ref)] -> ([(v, ref)], [(v, Name)])
partitionResolutions =
  partitionEithers . map f
  where
    f = \case
      (v, ResolvesToNamespace ref) -> Left (v, ref)
      (v, ResolvesToLocal name) -> Right (v, name)
