{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Unison.Codebase.Watch where

import Control.Concurrent
  ( forkIO,
    killThread,
    threadDelay,
  )
import Control.Concurrent.STM qualified as STM
import Data.Map qualified as Map
import Data.Time.Clock
  ( UTCTime,
    diffUTCTime,
  )
import System.FSNotify (Event (Added, Modified))
import System.FSNotify qualified as FSNotify
import Unison.Prelude
import Unison.Util.TQueue (TQueue)
import Unison.Util.TQueue qualified as TQueue
import UnliftIO.Exception (catch)
import UnliftIO.IORef
  ( newIORef,
    readIORef,
    writeIORef,
  )
import UnliftIO.MVar
  ( newEmptyMVar,
    putMVar,
    takeMVar,
    tryPutMVar,
    tryTakeMVar,
  )
import UnliftIO.STM (atomically)

untilJust :: (Monad m) => m (Maybe a) -> m a
untilJust act = act >>= maybe (untilJust act) return

watchDirectory' ::
  forall m. (MonadIO m) => FilePath -> m (IO (), IO (FilePath, UTCTime))
watchDirectory' d = do
  mvar <- newEmptyMVar
  let handler :: Event -> IO ()
      handler e = case e of
        Added fp t FSNotify.IsFile -> doIt fp t
        Modified fp t FSNotify.IsFile -> doIt fp t
        _ -> pure ()
        where
          doIt fp t = do
            _ <- tryTakeMVar mvar
            putMVar mvar (fp, t)
  -- janky: used to store the cancellation action returned
  -- by `watchDir`, which is created asynchronously
  cleanupRef <- newEmptyMVar
  -- we don't like FSNotify's debouncing (it seems to drop later events)
  -- so we will be doing our own instead
  let config = FSNotify.defaultConfig
  cancel <- liftIO $
    forkIO $
      FSNotify.withManagerConf config $ \mgr -> do
        cancelInner <- FSNotify.watchDir mgr d (const True) handler <|> (pure (pure ()))
        putMVar cleanupRef $ liftIO cancelInner
        forever $ threadDelay 1000000
  let cleanup :: IO ()
      cleanup = join (takeMVar cleanupRef) >> killThread cancel
  pure (cleanup, takeMVar mvar)

collectUntilPause :: forall a. TQueue a -> Int -> IO [a]
collectUntilPause queue minPauseµsec = do
  -- 1. wait for at least one element in the queue
  void . atomically $ TQueue.peek queue

  let go :: IO [a]
      go = do
        before <- atomically $ TQueue.enqueueCount queue
        threadDelay minPauseµsec
        after <- atomically $ TQueue.enqueueCount queue
        -- if nothing new is on the queue, then return the contents
        if before == after
          then atomically $ TQueue.flush queue
          else go
  go

watchDirectory ::
  forall m.
  (MonadIO m) =>
  FilePath ->
  (FilePath -> Bool) ->
  m (IO (), IO (FilePath, Text))
watchDirectory dir allow = do
  previousFiles <- newIORef Map.empty
  (cancelWatch, watcher) <- watchDirectory' dir
  let process :: FilePath -> UTCTime -> IO (Maybe (FilePath, Text))
      process file t =
        if allow file
          then
            let handle :: IOException -> IO ()
                handle _e =
                  -- Sometimes we notice a change and try to read a file while it's being written.
                  -- This typically occurs when UCM is writing to the scratch file and can be
                  -- ignored anyways.
                  pure ()
                go :: IO (Maybe (FilePath, Text))
                go = liftIO $ do
                  contents <- readUtf8 file
                  prevs <- readIORef previousFiles
                  case Map.lookup file prevs of
                    -- if the file's content's haven't changed and less than .5s
                    -- have elapsed, wait for the next update
                    Just (contents0, t0)
                      | contents == contents0 && (t `diffUTCTime` t0) < 0.5 ->
                          return Nothing
                    _ ->
                      Just (file, contents)
                        <$ writeIORef previousFiles (Map.insert file (contents, t) prevs)
             in catch go (\e -> Nothing <$ handle e)
          else return Nothing
  queue <- TQueue.newIO
  gate <- liftIO newEmptyMVar
  -- We spawn a separate thread to siphon the file change events
  -- into a queue, which can be debounced using `collectUntilPause`
  enqueuer <- liftIO . forkIO $ do
    takeMVar gate -- wait until gate open before starting
    forever $ do
      event@(file, _) <- watcher
      when (allow file) $
        STM.atomically $
          TQueue.enqueue queue event
  pending <- newIORef []
  let await :: IO (FilePath, Text)
      await =
        untilJust $
          readIORef pending >>= \case
            [] -> do
              -- open the gate
              _ <- tryPutMVar gate ()
              -- this debounces the events, waits for 50ms pause
              -- in file change events
              events <- collectUntilPause queue 50000
              -- traceM $ "Collected file change events" <> show events
              case events of
                [] -> pure Nothing
                -- we pick the last of the events within the 50ms window
                -- TODO: consider enqueing other events if there are
                -- multiple events for different files
                _ -> uncurry process $ last events
            ((file, t) : rest) -> do
              writeIORef pending rest
              process file t
      cancel = cancelWatch >> killThread enqueuer
  pure (cancel, await)
