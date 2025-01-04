-- | Helpers which are specific to the local share server.
module Unison.Server.Local (relocateToNameRoot) where

import Control.Lens hiding ((??))
import Control.Monad.Reader
import Control.Monad.Writer.Strict (WriterT, execWriterT, tell)
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import U.Codebase.Branch
import U.Codebase.Branch qualified as V2Branch
import U.Codebase.Causal qualified as Causal
import Unison.Codebase.Path
import Unison.Codebase.Path qualified as Path
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude
import Unison.Server.Backend
import Unison.Sqlite qualified as Sqlite
import Unison.Util.Recursion (Algebra, XNor (Both, Neither), cata)

-- | Given an arbitrary query and perspective, find the name root the query belongs in,
-- then return that root and the query relocated to that root.
--
-- A name root is either a project root or a dependency root.
-- E.g. @.myproject.some.namespace -> .myproject@ or @.myproject.lib.base.List -> .myproject.lib.base@
relocateToNameRoot ::
  Path.Absolute ->
  HQ.HashQualified Name ->
  V2Branch.Branch Sqlite.Transaction ->
  Sqlite.Transaction (Either BackendError (Path.Absolute, HQ.HashQualified Name))
relocateToNameRoot perspective query =
  let queryLocation = maybe perspective (Path.resolve perspective . Path.fromName') $ HQ.toName query
   in -- Names should be found from the project root of the queried name
      fmap
        ( maybe
            (Right (perspective, query))
            ( \projectRoot -> case Path.longestPathPrefix perspective projectRoot of
                -- The perspective is equal to the project root
                (_sharedPrefix, Path.Current, Path.Current) -> Right (perspective, query)
                -- The perspective is _outside_ of the project containing the query
                (_sharedPrefix, Path.Current, remainder) ->
                  -- Since the project root is lower down we need to strip the part of the prefix
                  -- which is now redundant.
                  Right $ (projectRoot, query <&> \n -> fromMaybe n $ Path.unprefixName remainder n)
                -- The namesRoot is _inside_ the project containing the query
                (_sharedPrefix, remainder, Path.Current) ->
                  -- Since the project is higher up, we need to prefix the query
                  -- with the remainder of the path
                  Right (projectRoot, query <&> Path.prefixNameIfRel (Path.RelativePath' remainder))
                -- The namesRoot and project root are disjoint, this shouldn't ever happen.
                (_, _, _) -> Left (DisjointProjectAndPerspective perspective projectRoot)
            )
        )
        . inferNamesRoot queryLocation

-- | When folded, this extracts a specific path prefix, plus one extra segment.
--
--   If the first argument is `Just`, the list is used to match subsequences that aren’t at the start of the list.
segsPlusOne :: Maybe [NameSegment] -> Algebra (XNor NameSegment) ([NameSegment] -> Maybe [NameSegment])
segsPlusOne unanchored = \case
  Neither -> const Nothing
  Both seg fn -> \case
    [] -> pure [seg]
    next : rest -> fmap (next :) . fn =<< if seg == next then pure rest else unanchored

-- | Infers path to use for loading names.
--
-- A name root is either a project root or a dependency root.
-- E.g. @.myproject.some.namespace -> .myproject@ (where .myproject.lib exists) or @.myproject.lib.base.List -> .myproject.lib.base@
inferNamesRoot :: Path.Absolute -> Branch Sqlite.Transaction -> Sqlite.Transaction (Maybe Path.Absolute)
inferNamesRoot p b
  | Just match <- findBaseProject p = pure $ Just match
  | Just depRoot <- findDepRoot (Path.unabsolute p) = pure . Just $ Path.Absolute depRoot
  | otherwise = fmap Path.Absolute . getLast <$> execWriterT (runReaderT (go (Path.unabsolute p) b) mempty)
  where
    findBaseProject :: Path.Absolute -> Maybe Path.Absolute
    findBaseProject =
      fmap (Path.Absolute . Path.fromList)
        . flip (cata $ segsPlusOne Nothing) [NameSegment.publicLooseCodeSegment, NameSegment.baseSegment]
    go :: Path -> Branch Sqlite.Transaction -> ReaderT Path (WriterT (Last Path) Sqlite.Transaction) ()
    go = cata \case
      Neither -> const $ pure ()
      Both nextChild fn -> \b -> do
        childMap <- lift . lift $ nonEmptyChildren b
        when (isJust $ Map.lookup NameSegment.libSegment childMap) $ ask >>= tell . Last . Just
        maybe (pure ()) (local (`Path.descend` nextChild) . fn <=< lift . lift . Causal.value) $
          Map.lookup (coerce nextChild) childMap

-- | If the provided path is within a lib dir (or a transitive lib) find the dependency
-- we're in.
--
-- E.g. @.myproject.lib.base.List -> .myproject.lib.base@
-- E.g. @.myproject.lib.distributed.lib.base.List -> .myproject.lib.distributed.lib.base@
--
-- >>> findDepRoot (Path.fromList ["myproject", "lib", "base", "List"])
-- Just myproject.lib.base
--
-- >>> findDepRoot (Path.fromList ["myproject", "lib", "distributed", "lib", "base", "List"])
-- Just myproject.lib.distributed.lib.base
--
-- Just lib isn't inside a dependency.
-- >>> findDepRoot (Path.fromList ["myproject", "lib"])
-- Nothing
findDepRoot :: Path -> Maybe Path
findDepRoot = fmap Path.fromList . flip (cata . segsPlusOne $ pure [NameSegment.libSegment]) [NameSegment.libSegment]
