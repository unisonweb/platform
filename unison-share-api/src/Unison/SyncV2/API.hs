{-# LANGUAGE DataKinds #-}

module Unison.SyncV2.API
  ( API,
    api,
    Routes (..),
  )
where

import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Unison.SyncV2.Types
import Unison.Util.Servant.CBOR (CBOR)

api :: Proxy API
api = Proxy

type API = NamedRoutes Routes

type DownloadEntitiesStream =
  -- | The causal hash the client needs. The server should provide it and all of its dependencies
  ReqBody '[CBOR, JSON] DownloadEntitiesRequest
    :> StreamPost NoFraming CBOR (SourceIO (CBORBytes DownloadEntitiesChunk))

data Routes mode = Routes
  { downloadEntitiesStream :: mode :- "entities" :> "download" :> DownloadEntitiesStream
  }
  deriving stock (Generic)
