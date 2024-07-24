{-# LANGUAGE DataKinds #-}

module Unison.Sync.API (API, api) where

import Data.Proxy
import Servant.API
import Unison.Sync.Types

api :: Proxy API
api = Proxy

type API =
  "path" :> "get" :> GetCausalHashByPathEndpoint
    :<|> "entities" :> "download" :> DownloadEntitiesEndpoint
    :<|> "entities" :> "upload" :> UploadEntitiesEndpoint

type GetCausalHashByPathEndpoint =
  ReqBody '[JSON] GetCausalHashByPathRequest
    :> Post '[JSON] GetCausalHashByPathResponse

type DownloadEntitiesEndpoint =
  ReqBody '[JSON] DownloadEntitiesRequest
    :> Post '[JSON] DownloadEntitiesResponse

type UploadEntitiesEndpoint =
  ReqBody '[JSON] UploadEntitiesRequest
    :> Post '[JSON] UploadEntitiesResponse
