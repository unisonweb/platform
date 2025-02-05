-- | Types used by the UCM client during sync.
module Unison.Share.Sync.Types
  ( CodeserverTransportError (..),
    GetCausalHashByPathError (..),
    PullError (..),
    SyncError (..),
  )
where

import Servant.Client qualified as Servant
import Unison.Prelude
import Unison.Sync.Types qualified as Share

-- | An error occurred while pulling code from Unison Share.
data PullError
  = PullError'DownloadEntities Share.DownloadEntitiesError
  | PullError'GetCausalHash GetCausalHashByPathError
  | PullError'NoHistoryAtPath Share.Path
  deriving stock (Show)

-- | An error occurred when getting causal hash by path.
data GetCausalHashByPathError
  = -- | The user does not have permission to read this path.
    GetCausalHashByPathErrorNoReadPermission Share.Path
  | -- | The repo info was invalid. (err, repoInfo)
    GetCausalHashByPathErrorInvalidRepoInfo Text Share.RepoInfo
  | -- | The user was not found.
    GetCausalHashByPathErrorUserNotFound Share.RepoInfo
  deriving (Show)

-- | Generic Codeserver transport errors
data CodeserverTransportError
  = DecodeFailure Text Servant.Response
  | -- We try to catch permission failures in the endpoint's response type, but if any slip
    -- through they'll be translated as a PermissionDenied.
    PermissionDenied Text
  | RateLimitExceeded
  | Timeout
  | Unauthenticated Servant.BaseUrl
  | UnexpectedResponse Servant.Response
  | UnreachableCodeserver Servant.BaseUrl
  | -- I wish Servant gave us more detail, but it's just Text. I don't think we ever hit these errors though.
    StreamingError Text
  deriving stock (Show)
  deriving anyclass (Exception)

data SyncError e
  = TransportError CodeserverTransportError
  | SyncError e
  deriving stock (Functor, Show)
