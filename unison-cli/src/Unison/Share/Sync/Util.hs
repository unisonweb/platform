module Unison.Share.Sync.Util
  ( BailT (..),
    MonadBail (..),
    runBailT,
    mapBailT,
    withError,
  )
where

import Control.Monad.Reader (MonadReader (..), ReaderT (..), mapReaderT, withReaderT)
import Data.Data (Typeable)
import UnliftIO qualified as IO

newtype Handler e = Handler {runHandler :: forall x. e -> IO x}

newtype BailT e m a = BailT {unErrGroupT :: ReaderT (Handler e) m a}
  deriving newtype (Functor, Applicative, Monad, IO.MonadUnliftIO, IO.MonadIO)

newtype ExceptionWrapper e = ExceptionWrapper {unException :: e}

instance Show (ExceptionWrapper e) where
  show (ExceptionWrapper _) = "ExceptionWrapper<>"

instance (Typeable e) => IO.Exception (ExceptionWrapper e)

class MonadBail e m where
  bail :: e -> m a

mapBailT :: (Monad n) => (m a -> n b) -> BailT e m a -> BailT e n b
mapBailT f (BailT m) = BailT $ mapReaderT f $ m

withError :: (Monad m) => (e' -> e) -> BailT e' m a -> BailT e m a
withError f (BailT m) = BailT $ withReaderT (\h -> Handler $ runHandler h . f) m

instance (IO.MonadUnliftIO m, Typeable e) => MonadBail e (BailT e m) where
  bail e = do
    handler <- BailT ask
    BailT $ IO.liftIO $ runHandler handler e

runBailT :: (IO.MonadUnliftIO m, Typeable e) => BailT e m a -> (e -> m a) -> m a
runBailT (BailT m) handler = do
  IO.handle (handler . unException) $ runReaderT m (Handler (IO.throwIO . ExceptionWrapper))
