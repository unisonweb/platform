{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Unison.Codebase.Watch where

import           Control.Applicative
import           Control.Concurrent             ( forkIO
                                                , threadDelay
                                                , killThread
                                                )
import           Control.Concurrent.MVar
import           Control.Concurrent.STM         ( atomically )
import           Control.Monad                  ( forever
                                                , void
                                                , join
                                                )
import           Data.IORef
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO
import           Data.Time.Clock                ( UTCTime
                                                , diffUTCTime
                                                )
import qualified System.Console.Terminal.Size  as TerminalSize
import           System.FSNotify                ( Event(Added, Modified)
                                                , watchTree
                                                , withManager
                                                )
import           Unison.Names                   ( Names )
import qualified Unison.TermPrinter             as TermPrinter
import           Unison.Term                    ( Term )
import qualified Unison.PrettyPrintEnv         as PPE
import           Unison.Util.TQueue             ( TQueue )
import qualified Unison.Util.TQueue            as TQueue
import           Unison.Var                     ( Var )
-- import Debug.Trace

watchDirectory' :: FilePath -> IO (IO (), IO (FilePath, UTCTime))
watchDirectory' d = do
  mvar <- newEmptyMVar
  let doIt fp t = do
        _ <- tryTakeMVar mvar
        putMVar mvar (fp, t)
      handler e = case e of
        Added    fp t False -> doIt fp t
        Modified fp t False -> doIt fp t
        _                   -> pure ()
  -- janky: used to store the cancellation action returned
  -- by `watchTree`, which is created asynchronously
  cleanupRef <- newEmptyMVar
  cancel <- forkIO . withManager $ \mgr -> do
    cancelInner <- watchTree mgr d (const True) handler <|> (pure (pure ()))
    putMVar cleanupRef cancelInner
    forever $ threadDelay 1000000
  let cleanup = join (takeMVar cleanupRef) >> killThread cancel
  pure (cleanup, takeMVar mvar)

collectUntilPause :: TQueue a -> Int -> IO [a]
collectUntilPause queue minPauseµsec = do
-- 1. wait for at least one element in the queue
  void . atomically $ TQueue.peek queue

  let go = do
        before <- atomically $ TQueue.enqueueCount queue
        threadDelay minPauseµsec
        after <- atomically $ TQueue.enqueueCount queue
        -- if nothing new is on the queue, then return the contents
        if before == after
          then do
            atomically $ TQueue.flush queue
          else go
  go

-- TODO: Return a way of cancelling the watch
watchDirectory
  :: FilePath -> (FilePath -> Bool) -> IO (IO (), IO (FilePath, Text))
watchDirectory dir allow = do
  previousFiles <- newIORef Map.empty
  (cancel, watcher) <- watchDirectory' dir
  let
    await = do
      (file, t) <- watcher
      if allow file
        then do
          contents <- Data.Text.IO.readFile file
          prevs    <- readIORef previousFiles
          case Map.lookup file prevs of
            -- if the file's content's haven't changed and less than a second has passed,
            -- wait for the next update
            Just (contents0, t0)
              | contents == contents0 && (t `diffUTCTime` t0) < 1 -> await
            _ -> (file, contents) <$ writeIORef
              previousFiles
              (Map.insert file (contents, t) prevs)
        else await
  pure (cancel, await)

watchPrinter :: Var v => Names -> Text -> Term v -> IO ()
watchPrinter names label term = do
  -- I guess this string constant comes from somewhere, and we are using
  -- a bunch of spaces of the same total length.
  let lead = const ' ' <$> "      | > "
  -- weird that this doesn't incorporate the previous constant somehow
  let arr = "          ⧩"
  terminal  <- TerminalSize.size :: IO (Maybe (TerminalSize.Window Int)) 
  let width = maybe 80 TerminalSize.width terminal
  let tm = TermPrinter.pretty' (Just width) (PPE.fromNames names) term
  let tm2 = tm >>= \case
       '\n' -> '\n' : lead
       c -> pure c
  putStrLn $ Text.unpack label
  putStrLn arr
  putStrLn $ lead ++ tm2 ++ "\n"
