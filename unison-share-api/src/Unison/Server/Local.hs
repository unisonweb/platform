-- | Helpers which are specific to the local share server.
module Unison.Server.Local (relocateToNameRoot) where

import Control.Lens hiding ((??))
import Control.Lens.Cons qualified as Cons
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
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude
import Unison.Server.Backend
import Unison.Sqlite qualified as Sqlite

-- | Given an arbitrary query and perspective, find the name root the query belongs in,
-- then return that root and the query relocated to that root.
--
-- A name root is either a project root or a dependency root.
-- E.g. @.myproject.some.namespace -> .myproject@ or @.myproject.lib.base.List -> .myproject.lib.base@
relocateToNameRoot :: Path -> HQ.HashQualified Name -> V2Branch.Branch Sqlite.Transaction -> Sqlite.Transaction (Either BackendError (Path, HQ.HashQualified Name))
relocateToNameRoot perspective query rootBranch = do
  let queryLocation = HQ.toName query & maybe perspective \name -> perspective <> Path.fromName name
  -- Names should be found from the project root of the queried name
  (inferNamesRoot queryLocation rootBranch) >>= \case
    Nothing -> do
      pure $ Right (perspective, query)
    Just projectRoot ->
      case Path.longestPathPrefix perspective projectRoot of
        -- The perspective is equal to the project root
        (_sharedPrefix, Path.Empty, Path.Empty) -> do
          pure $ Right (perspective, query)
        -- The perspective is _outside_ of the project containing the query
        (_sharedPrefix, Path.Empty, remainder) -> do
          -- Since the project root is lower down we need to strip the part of the prefix
          -- which is now redundant.
          pure . Right $ (projectRoot, query <&> \n -> fromMaybe n $ Path.unprefixName (Path.Absolute remainder) n)
        -- The namesRoot is _inside (or equal to)_ the project containing the query
        (_sharedPrefix, remainder, Path.Empty) -> do
          -- Since the project is higher up, we need to prefix the query
          -- with the remainder of the path
          pure $ Right (projectRoot, query <&> Path.prefixNameIfRel (Path.RelativePath' $ Path.Relative remainder))
        -- The namesRoot and project root are disjoint, this shouldn't ever happen.
        (_, _, _) -> pure $ Left (DisjointProjectAndPerspective perspective projectRoot)

-- | Infers path to use for loading names.
--
-- A name root is either a project root or a dependency root.
-- E.g. @.myproject.some.namespace -> .myproject@ (where .myproject.lib exists) or @.myproject.lib.base.List -> .myproject.lib.base@
inferNamesRoot :: Path -> Branch Sqlite.Transaction -> Sqlite.Transaction (Maybe Path)
inferNamesRoot p b
  | Just match <- findBaseProject p = pure $ Just match
  | Just depRoot <- findDepRoot p = pure $ Just depRoot
  | otherwise = getLast <$> execWriterT (runReaderT (go p b) mempty)
  where
    findBaseProject :: Path -> Maybe Path
    findBaseProject
      (public Cons.:< base Cons.:< release Cons.:< _rest) =
        if public == NameSegment.publicLooseCodeSegment && base == NameSegment.baseSegment
          then Just (Path.fromList [public, base, release])
          else Nothing
    findBaseProject _ = Nothing
    go :: Path -> Branch Sqlite.Transaction -> ReaderT Path (WriterT (Last Path) Sqlite.Transaction) ()
    go p b = do
      childMap <- lift . lift $ nonEmptyChildren b
      when (isJust $ Map.lookup NameSegment.libSegment childMap) $ ask >>= tell . Last . Just
      case p of
        Path.Empty -> pure ()
        (nextChild Cons.:< pathRemainder) ->
          case Map.lookup (coerce nextChild) childMap of
            Nothing -> pure ()
            Just childCausal -> do
              childBranch <- lift . lift $ Causal.value childCausal
              local (`Path.descend` nextChild) (go pathRemainder childBranch)

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
findDepRoot (lib Cons.:< depRoot Cons.:< rest)
  | lib == NameSegment.libSegment =
      -- Keep looking to see if the full path is actually in a transitive dependency, otherwise
      -- fallback to this spot
      ((Path.fromList [lib, depRoot] <>) <$> findDepRoot rest)
        <|> Just (Path.fromList [lib, depRoot])
findDepRoot (other Cons.:< rest) = (other Cons.:<) <$> findDepRoot rest
findDepRoot _ = Nothing
