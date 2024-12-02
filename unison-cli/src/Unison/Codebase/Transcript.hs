{-# LANGUAGE PatternSynonyms #-}

-- | The data model for Unison transcripts.
module Unison.Codebase.Transcript
  ( ExpectingError,
    ScratchFileName,
    Hidden (..),
    UcmLine (..),
    UcmContext (..),
    APIRequest (..),
    pattern CMarkCodeBlock,
    Stanza,
    InfoTags (..),
    defaultInfoTags,
    defaultInfoTags',
    ProcessedBlock (..),
    CMark.Node,
  )
where

import CMark qualified
import Unison.Core.Project (ProjectBranchName, ProjectName)
import Unison.Prelude
import Unison.Project (ProjectAndBranch)

type ExpectingError = Bool

type ScratchFileName = Text

data Hidden = Shown | HideOutput | HideAll
  deriving (Eq, Ord, Read, Show)

data UcmLine
  = UcmCommand UcmContext Text
  | -- | Text does not include the '--' prefix.
    UcmComment Text
  | UcmOutputLine Text
  deriving (Eq, Show)

-- | Where a command is run: a project branch (myproject/mybranch>).
data UcmContext
  = UcmContextProject (ProjectAndBranch ProjectName ProjectBranchName)
  deriving (Eq, Show)

data APIRequest
  = GetRequest Text
  | APIComment Text
  | APIResponseLine Text
  deriving (Eq, Show)

pattern CMarkCodeBlock :: (Maybe CMark.PosInfo) -> Text -> Text -> CMark.Node
pattern CMarkCodeBlock pos info body = CMark.Node pos (CMark.CODE_BLOCK info body) []

type Stanza = Either CMark.Node ProcessedBlock

data InfoTags a = InfoTags
  { hidden :: Hidden,
    expectingError :: ExpectingError,
    generated :: Bool,
    additionalTags :: a
  }
  deriving (Eq, Ord, Read, Show)

defaultInfoTags :: a -> InfoTags a
defaultInfoTags = InfoTags Shown False False

-- | If the `additionalTags` form a `Monoid`, then you don’t need to provide a default value for them.
defaultInfoTags' :: (Monoid a) => InfoTags a
defaultInfoTags' = defaultInfoTags mempty

data ProcessedBlock
  = Ucm (InfoTags ()) [UcmLine]
  | Unison (InfoTags (Maybe ScratchFileName)) Text
  | API (InfoTags ()) [APIRequest]
  deriving (Eq, Show)
