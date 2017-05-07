module Control.Monad.Trans.Control
  ( class MonadTransControl
  , liftWith
  , restoreT
  , class MonadBaseControl
  , liftBaseWith
  , restoreM
  , defaultLiftBaseWith
  , defaultRestoreM
  , WriterTStT (..)
  , writerTStTToTuple, tupleToWriterTStT
  ) where

import Prelude
import Data.Functor.Compose (Compose (..))
import Data.Either (Either (..))
import Data.Tuple (Tuple (..))
import Data.Maybe (Maybe (..))
import Data.Identity (Identity (..))
import Data.List (List (..))
import Data.Monoid (class Monoid, mempty)
import Control.Monad.Base (class MonadBase, liftBase)
import Control.Monad.Eff (Eff)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Reader.Trans (ReaderT (..))
import Control.Monad.Writer.Trans (WriterT (..), runWriterT)
import Control.Monad.State.Trans (StateT (..), runStateT)
import Control.Monad.Cont.Trans (ContT)
import Control.Monad.Except.Trans (ExceptT (..), runExceptT)
-- import Control.Monad.List.Trans (ListT (..))
import Control.Monad.Maybe.Trans (MaybeT (..), runMaybeT)
import Control.Monad.RWS.Trans (RWST (..), runRWST, RWSResult (..))



class MonadTrans t <= MonadTransControl t stT | t -> stT where
  liftWith :: forall m b. Monad m => ((forall a. t m a -> m (stT a)) -> m b) -> t m b
  restoreT :: forall m a. Monad m => m (stT a) -> t m a



instance readerTMonadTransControl :: MonadTransControl (ReaderT r) Identity where
  liftWith f = ReaderT \r -> f \(ReaderT g) -> Identity <$> g r
  restoreT x = runIdentity <$> lift x


data WriterTStT w a = WriterTStT w a

writerTStTToTuple :: forall w a. WriterTStT w a -> Tuple a w
writerTStTToTuple (WriterTStT w a) = Tuple a w

tupleToWriterTStT :: forall w a. Tuple a w -> WriterTStT w a
tupleToWriterTStT (Tuple a w) = WriterTStT w a


instance writerTMonadTransControl :: Monoid r => MonadTransControl (WriterT r) (WriterTStT r) where
  liftWith f = lift (f (\x -> tupleToWriterTStT <$> runWriterT x))
  restoreT x = WriterT (writerTStTToTuple <$> x)

instance stateTMonadTransControl :: MonadTransControl (StateT r) (WriterTStT r) where
  liftWith f = StateT \s -> (\b -> Tuple b s) <$> f (\x -> tupleToWriterTStT <$> runStateT x s)
  restoreT x = StateT \_ -> (writerTStTToTuple <$> x)

instance exceptTMonadTransControl :: MonadTransControl (ExceptT r) (Either r) where
  liftWith f = ExceptT $ Right <$> f runExceptT
  restoreT = ExceptT

-- FIXME 0.11.0's -transformers library actually exports the goods >.>
-- instance listTMonadTransControl :: MonadTransControl ListT List where
--   liftWith f = ListT $ (\x -> Cons x Nil) <$> f (\(ListT x) -> x)
--   restoreT = ListT

instance maybeTMonadTransControl :: MonadTransControl MaybeT Maybe where
  liftWith f = MaybeT $ Just <$> f runMaybeT
  restoreT = MaybeT

instance rwsTMonadTransControl :: Monoid w => MonadTransControl (RWST r w s) (Compose (Tuple w) (Tuple s))  where
  liftWith f = RWST \r s -> (\x -> RWSResult s x mempty) <$> f (\t -> (\(RWSResult s a w) -> Compose (Tuple w (Tuple s a))) <$> runRWST t r s)
  restoreT mSt = RWST \_ _ -> (\(Compose (Tuple w (Tuple s a))) -> RWSResult s a w) <$> mSt




class MonadBase base m <= MonadBaseControl base m stM | m -> stM base where
  liftBaseWith :: forall b. ((forall a. m a -> base (stM a)) -> base b) -> m b
  restoreM     :: forall a. base (stM a) -> m a


instance effMonadBaseControl :: MonadBaseControl (Eff e) (Eff e) Identity where
  liftBaseWith f = f (map Identity)
  restoreM = (map runIdentity)

instance eitherMonadBaseControl :: MonadBaseControl (Either e) (Either e) Identity where
  liftBaseWith f = f (map Identity)
  restoreM = (map runIdentity)

instance tupleMonadBaseControl :: (Monoid e) => MonadBaseControl (Tuple e) (Tuple e) Identity where
  liftBaseWith f = f (map Identity)
  restoreM = (map runIdentity)

instance maybeMonadBaseControl :: MonadBaseControl Maybe Maybe Identity where
  liftBaseWith f = f (map Identity)
  restoreM = (map runIdentity)

instance identityMonadBaseControl :: MonadBaseControl Identity Identity Identity where
  liftBaseWith f = f (map Identity)
  restoreM = (map runIdentity)

instance listMonadBaseControl :: MonadBaseControl List List Identity where
  liftBaseWith f = f (map Identity)
  restoreM = (map runIdentity)

instance arrayMonadBaseControl :: MonadBaseControl Array Array Identity where
  liftBaseWith f = f (map Identity)
  restoreM = (map runIdentity)

instance funcMonadBaseControl :: MonadBaseControl ((->) r) ((->) r) Identity where
  liftBaseWith f = f (map Identity)
  restoreM = (map runIdentity)

instance readerTMonadBaseControl :: (MonadBaseControl base m stM, Monad m, Monad base) => MonadBaseControl base (ReaderT r m) (Compose stM Identity) where
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance writerTMonadBaseControl :: (MonadBaseControl base m stM, Monad m, Monad base, Monoid r) => MonadBaseControl base (WriterT r m) (Compose stM (WriterTStT r)) where
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance stateTMonadBaseControl :: (MonadBaseControl base m stM, Monad m, Monad base) => MonadBaseControl base (StateT r m) (Compose stM (WriterTStT r)) where
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance exceptTMonadBaseControl :: (MonadBaseControl base m stM, Monad m, Monad base) => MonadBaseControl base (ExceptT r m) (Compose stM (Either r)) where
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

-- instance listTMonadBaseControl :: (MonadBaseControl base m stM, Monad m, Monad base) => MonadBaseControl base (ListT m) (Compose stM Identity) where
--   liftBaseWith = defaultLiftBaseWith
--   restoreM = defaultRestoreM

instance maybeTMonadBaseControl :: (MonadBaseControl base m stM, Monad m, Monad base) => MonadBaseControl base (MaybeT m) (Compose stM Maybe) where
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance rwsTMonadBaseControl :: (MonadBaseControl base m stM, Monad m, Monad base, Monoid w) => MonadBaseControl base (RWST r w s m) (Compose stM (Compose (Tuple w) (Tuple s))) where
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM





defaultLiftBaseWith :: forall base m t stM stT b
                     . ( MonadBaseControl base m stM
                       , Monad m
                       , Monad base
                       , MonadTrans t
                       , MonadTransControl t stT
                       )
                    => -- MonadBaseControl base (t m) (Compose stM stT)
                       ((forall a. t m a -> base (Compose stM stT a)) -> base b) -> t m b
defaultLiftBaseWith f = liftWith \run -> liftBaseWith \runInBase -> f (\x -> Compose <$> runInBase (run x))


defaultRestoreM :: forall base m t stM stT a
                 . ( MonadBaseControl base m stM
                   , Monad m
                   , Monad base
                   , MonadTrans t
                   , MonadTransControl t stT
                   )
                => base (Compose stM stT a) -> t m a
defaultRestoreM x = restoreT (restoreM (runCompose <$> x))


runCompose :: forall f g a. Compose f g a -> f (g a)
runCompose (Compose x) = x

runIdentity :: forall a. Identity a -> a
runIdentity (Identity x) = x
