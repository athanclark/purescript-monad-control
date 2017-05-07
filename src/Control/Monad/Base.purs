module Control.Monad.Base where

import Prelude
import Data.Either (Either)
import Data.Tuple (Tuple)
import Data.Maybe (Maybe)
import Data.Identity (Identity)
import Data.List (List)
import Data.Monoid (class Monoid)
import Control.Monad.Eff (Eff)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.Writer.Trans (WriterT)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Cont.Trans (ContT)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.List.Trans (ListT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.RWS.Trans (RWST)


class (Monad b, Monad m) <= MonadBase b m | m -> b where
  liftBase :: forall a. b a -> m a

instance effMonadBase :: MonadBase (Eff e) (Eff e) where
  liftBase = id

instance eitherMonadBase :: MonadBase (Either e) (Either e) where
  liftBase = id

instance tupleMonadBase :: Monoid e => MonadBase (Tuple e) (Tuple e) where
  liftBase = id

instance maybeMonadBase :: MonadBase Maybe Maybe where
  liftBase = id

instance identityMonadBase :: MonadBase Identity Identity where
  liftBase = id

instance listMonadBase :: MonadBase List List where
  liftBase = id

instance arrayMonadBase :: MonadBase Array Array where
  liftBase = id

instance functionMonadBase :: MonadBase ((->) r) ((->) r) where
  liftBase = id



instance readerTMonadBase :: (MonadBase b m, Monad m, Monad b) => MonadBase b (ReaderT r m) where
  liftBase x = lift (liftBase x)

instance writerTMonadBase :: (MonadBase b m, Monad m, Monad b, Monoid r) => MonadBase b (WriterT r m) where
  liftBase x = lift (liftBase x)

instance stateTMonadBase :: (MonadBase b m, Monad m, Monad b) => MonadBase b (StateT r m) where
  liftBase x = lift (liftBase x)

instance contTMonadBase :: (MonadBase b m, Monad m, Monad b) => MonadBase b (ContT r m) where
  liftBase x = lift (liftBase x)

instance exceptTMonadBase :: (MonadBase b m, Monad m, Monad b) => MonadBase b (ExceptT r m) where
  liftBase x = lift (liftBase x)

instance listTMonadBase :: (MonadBase b m, Monad m, Monad b) => MonadBase b (ListT m) where
  liftBase x = lift (liftBase x)

instance maybeTMonadBase :: (MonadBase b m, Monad m, Monad b) => MonadBase b (MaybeT m) where
  liftBase x = lift (liftBase x)

instance rwsTMonadBase :: (MonadBase b m, Monad m, Monad b, Monoid w) => MonadBase b (RWST r w s m) where
  liftBase x = lift (liftBase x)
