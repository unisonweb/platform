module Unison.Codebase.Editor.RemoteRepo where

import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Share.Types
import Unison.Util.Recursion qualified as Rec

data ShareCodeserver
  = DefaultCodeserver
  | CustomCodeserver CodeserverURI
  deriving stock (Eq, Ord, Show)

newtype ShareUserHandle = ShareUserHandle {shareUserHandleToText :: Text}
  deriving stock (Eq, Ord, Show)

-- |
-- >>> :set -XOverloadedLists
-- >>> import Data.Maybe (fromJust)
-- >>> import Network.URI
-- >>> displayShareCodeserver DefaultCodeserver "share" ["base", "List"]
-- "share.base.List"
-- >>> displayShareCodeserver DefaultCodeserver "share" []
-- "share"
-- >>> displayShareCodeserver (CustomCodeserver . fromJust $ parseURI "https://share-next.unison-lang.org/api" >>= codeserverFromURI ) "unison" ["base", "List"]
-- "share(https://share-next.unison-lang.org:443/api).unison.base.List"
displayShareCodeserver :: ShareCodeserver -> ShareUserHandle -> Path -> Text
displayShareCodeserver cs shareUser path =
  let shareServer = case cs of
        DefaultCodeserver -> ""
        CustomCodeserver cu -> "share(" <> tShow cu <> ")."
   in shareServer <> shareUserHandleToText shareUser <> maybePrintPath path

-- | print remote namespace
printReadRemoteNamespace :: (a -> Text) -> ReadRemoteNamespace a -> Text
printReadRemoteNamespace printProject = \case
  ReadShare'LooseCode ReadShareLooseCode {server, repo, path} -> displayShareCodeserver server repo path
  ReadShare'ProjectBranch project -> printProject project

-- | Render a 'WriteRemoteNamespace' as text.
printWriteRemoteNamespace :: (ProjectAndBranch ProjectName ProjectBranchName) -> Text
printWriteRemoteNamespace projectAndBranch = into @Text projectAndBranch

maybePrintPath :: Path -> Text
maybePrintPath path =
  if path == mempty
    then mempty
    else "." <> Path.toText path

data ReadRemoteNamespace a
  = ReadShare'LooseCode !ReadShareLooseCode
  | -- | A remote project+branch, specified by name (e.g. @unison/base/main).
    -- Currently assumed to be hosted on Share, though we could include a ShareCodeserver in here, too.
    ReadShare'ProjectBranch !a
  deriving stock (Eq, Functor, Show, Generic)

data ReadShareLooseCode = ReadShareLooseCode
  { server :: !ShareCodeserver,
    repo :: !ShareUserHandle,
    -- sch :: Maybe ShortCausalHash, -- maybe later
    path :: !Path
  }
  deriving stock (Eq, Show)

isPublic :: ReadShareLooseCode -> Bool
isPublic ReadShareLooseCode {path} = case Rec.project path of
  Rec.Neither -> False
  Rec.Both segment _ -> segment == NameSegment.publicLooseCodeSegment
