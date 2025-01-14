module Unison.Util.TQueue where

import Control.Concurrent.Async qualified as Async
import Data.Sequence (Seq ((:<|)), (|>))
import Data.Sequence qualified as S
import Unison.Prelude
import UnliftIO.STM hiding (TQueue)

data TQueue a = TQueue (TVar (Seq a)) (TVar Word64)

prepopulatedIO :: forall a m. (MonadIO m) => Seq a -> m (TQueue a)
prepopulatedIO as = TQueue <$> newTVarIO as <*> newTVarIO (fromIntegral $ length as)

newIO :: forall a m. (MonadIO m) => m (TQueue a)
newIO = prepopulatedIO mempty

size :: TQueue a -> STM Int
size (TQueue q _) = S.length <$> readTVar q

-- Waits for this queue to reach a size <= target.
-- Consumes no elements; it's expected there is some
-- other thread which is consuming elements from the queue.
awaitSize :: Int -> TQueue a -> STM ()
awaitSize target q =
  size q >>= \n ->
    if n <= target
      then pure ()
      else retrySTM

peek :: TQueue a -> STM a
peek (TQueue v _) =
  readTVar v >>= \case
    a :<| _ -> pure a
    _ -> retrySTM

dequeue :: TQueue a -> STM a
dequeue (TQueue v _) =
  readTVar v >>= \case
    a :<| as -> writeTVar v as *> pure a
    _ -> retrySTM

undequeue :: TQueue a -> a -> STM ()
undequeue (TQueue v _) a =
  readTVar v >>= \as -> writeTVar v (a :<| as)

tryDequeue :: TQueue a -> STM (Maybe a)
tryDequeue (TQueue v _) =
  readTVar v >>= \case
    a :<| as -> writeTVar v as *> pure (Just a)
    _ -> pure Nothing

dequeueN :: TQueue a -> Int -> STM [a]
dequeueN (TQueue v _) n =
  readTVar v >>= \s ->
    if length s >= n
      then writeTVar v (S.drop n s) $> toList (S.take n s)
      else retrySTM

-- return the number of enqueues over the life of the queue
enqueueCount :: TQueue a -> STM Word64
enqueueCount (TQueue _ count) = readTVar count

flush :: TQueue a -> STM [a]
flush (TQueue v _) = do
  s <- readTVar v
  writeTVar v mempty
  pure . toList $ s

enqueue :: TQueue a -> a -> STM ()
enqueue (TQueue v count) a = do
  modifyTVar' v (|> a)
  modifyTVar' count (+ 1)

raceIO :: (MonadIO m) => STM a -> STM b -> m (Either a b)
raceIO a b = liftIO do
  aa <- Async.async $ atomically a
  ab <- Async.async $ atomically b
  Async.waitEitherCancel aa ab

-- take all elements up to but not including the first not satisfying cond
tryPeekWhile :: (a -> Bool) -> TQueue a -> STM [a]
tryPeekWhile cond (TQueue v _) = toList . S.takeWhileL cond <$> readTVar v

-- block until at least one element is enqueued not satisfying cond,
-- then return the prefix before that
takeWhile :: (a -> Bool) -> TQueue a -> STM [a]
takeWhile cond (TQueue v _) =
  readTVar v >>= \s ->
    let (left, right) = S.spanl cond s
     in if null right
          then retrySTM
          else writeTVar v right $> toList left

peekWhile :: (a -> Bool) -> TQueue a -> STM [a]
peekWhile cond (TQueue v _) =
  readTVar v >>= \s ->
    let (left, right) = S.spanl cond s
     in if null right
          then retrySTM
          else pure $ toList left
