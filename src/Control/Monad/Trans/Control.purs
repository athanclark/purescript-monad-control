module Control.Monad.Trans.Control
  ( class MonadTransControl
  , liftWith
  , restoreT
  , integrateT
  , class MonadBaseControl
  , liftBaseWith
  , restoreM
  , integrateM
  , defaultLiftBaseWith
  , defaultRestoreM
  , WriterTStT (..)
  , writerTStTToTuple, tupleToWriterTStT
  , FreeTStT (..)
  , freeTStTToEither, eitherToFreeTStT
  ) where

import Prelude
import Data.Functor.Compose (Compose (..))
import Data.Either (Either (..))
import Data.Tuple (Tuple (..))
import Data.Maybe (Maybe (..))
import Data.Identity (Identity (..))
import Data.List (List)
import Control.Monad.Base (class MonadBase)
import Effect.Aff (Aff)
import Effect (Effect)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Reader.Trans (ReaderT (..))
import Control.Monad.Writer.Trans (WriterT (..), runWriterT)
import Control.Monad.State.Trans (StateT (..), runStateT)
import Control.Monad.Except.Trans (ExceptT (..), runExceptT)
-- import Control.Monad.List.Trans (ListT (..))
import Control.Monad.Maybe.Trans (MaybeT (..), runMaybeT)
import Control.Monad.RWS.Trans (RWST (..), runRWST, RWSResult (..))
import Control.Monad.Free.Trans (FreeT, freeT, resume)
import Control.Monad.Rec.Class (class MonadRec)



class (MonadTrans t, Monad m) <= MonadTransControl m t stT | t -> stT where
  liftWith :: forall b. ((forall a. t m a -> m (stT a)) -> m b) -> t m b
  restoreT :: forall a. m (stT a) -> t m a


-- | Pack a state belonging to `t` back into it, instead of throwing it away
integrateT :: forall m t stT a. Bind (t m) => MonadTransControl m t stT => t m (stT a) -> t m a
integrateT x = join ((restoreT <<< pure) <$> x)


instance readerTMonadTransControl :: Monad m => MonadTransControl m (ReaderT r) Identity where
  liftWith f = ReaderT \r -> f \(ReaderT g) -> Identity <$> g r
  restoreT x = runIdentity <$> lift x


data WriterTStT w a = WriterTStT w a

instance functorWriterTStT :: Functor (WriterTStT w) where
  map f (WriterTStT w x) = WriterTStT w (f x)

writerTStTToTuple :: forall w a. WriterTStT w a -> Tuple a w
writerTStTToTuple (WriterTStT w a) = Tuple a w

tupleToWriterTStT :: forall w a. Tuple a w -> WriterTStT w a
tupleToWriterTStT (Tuple a w) = WriterTStT w a


-- liftEffectWith :: ((Aff eff a -> Eff eff a) -> Eff eff a) -> Aff eff a
-- liftEffectWith f = makeAff \onError onSuccess ->
--   f (\a -> runAff onError onSuccess a)


newtype FreeTStT f m a = FreeTStT (Either a (f (FreeT f m a)))

instance functorFreeTStT :: (Functor f, Monad m) => Functor (FreeTStT f m) where
  map f (FreeTStT e) = FreeTStT $ case e of
    Left x -> Left (f x)
    Right x -> Right (map (map f) x)

freeTStTToEither :: forall f m a. FreeTStT f m a -> Either a (f (FreeT f m a))
freeTStTToEither (FreeTStT x) = x

eitherToFreeTStT :: forall f m a. Either a (f (FreeT f m a)) -> FreeTStT f m a
eitherToFreeTStT x = FreeTStT x


instance freeTMonadTransControl :: (Functor f, MonadRec m) => MonadTransControl m (FreeT f) (FreeTStT f m) where
  liftWith f =
    freeT \_ -> Left <$> f (\x -> eitherToFreeTStT <$> resume x)
  restoreT x = freeT \_ -> freeTStTToEither <$> x


instance writerTMonadTransControl :: (Monoid r, Monad m) => MonadTransControl m (WriterT r) (WriterTStT r) where
  liftWith f = lift (f (\x -> tupleToWriterTStT <$> runWriterT x))
  restoreT x = WriterT (writerTStTToTuple <$> x)

instance stateTMonadTransControl :: Monad m => MonadTransControl m (StateT r) (WriterTStT r) where
  liftWith f = StateT \s -> (\b -> Tuple b s) <$> f (\x -> tupleToWriterTStT <$> runStateT x s)
  restoreT x = StateT \_ -> (writerTStTToTuple <$> x)

instance exceptTMonadTransControl :: Monad m => MonadTransControl m (ExceptT r) (Either r) where
  liftWith f = ExceptT $ Right <$> f runExceptT
  restoreT = ExceptT

-- FIXME 0.11.0's -transformers library actually exports the goods >.>
-- instance listTMonadTransControl :: MonadTransControl ListT List where
--   liftWith f = ListT $ (\x -> Cons x Nil) <$> f (\(ListT x) -> x)
--   restoreT = ListT

instance maybeTMonadTransControl :: Monad m => MonadTransControl m MaybeT Maybe where
  liftWith f = MaybeT $ Just <$> f runMaybeT
  restoreT = MaybeT

instance rwsTMonadTransControl :: (Monoid w, Monad m) => MonadTransControl m (RWST r w s) (Compose (Tuple w) (Tuple s))  where
  liftWith f = RWST \r s -> (\x -> RWSResult s x mempty) <$> f (\t -> (\(RWSResult s' a w) -> Compose (Tuple w (Tuple s' a))) <$> runRWST t r s)
  restoreT mSt = RWST \_ _ -> (\(Compose (Tuple w (Tuple s a))) -> RWSResult s a w) <$> mSt




class MonadBase base m <= MonadBaseControl base m stM | m -> stM base where
  liftBaseWith :: forall b. ((forall a. m a -> base (stM a)) -> base b) -> m b
  restoreM     :: forall a. base (stM a) -> m a

-- | Pack a state belonging to `m` back into it, instead of throwing it away
integrateM :: forall base m stM a. Applicative base => MonadBaseControl base m stM => m (stM a) -> m a
integrateM x = join ((restoreM <<< pure) <$> x)

instance affMonadBaseControl :: MonadBaseControl Aff Aff Identity where
  liftBaseWith f = f (map Identity)
  restoreM = (map runIdentity)

instance effMonadBaseControl :: MonadBaseControl Effect Effect Identity where
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

instance freeTMonadBaseControl :: (MonadBaseControl base m stM, Monad base, Functor f, MonadRec m) => MonadBaseControl base (FreeT f m) (Compose stM (FreeTStT f m)) where
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM



defaultLiftBaseWith :: forall base m t stM stT b
                     . MonadBaseControl base m stM
                    => Monad m
                    => Monad base
                    => MonadTrans t
                    => MonadTransControl m t stT
                    => -- MonadBaseControl base (t m) (Compose stM stT)
                       ((forall a. t m a -> base (Compose stM stT a)) -> base b) -> t m b
defaultLiftBaseWith f = liftWith \run -> liftBaseWith \runInBase -> f (\x -> Compose <$> runInBase (run x))


defaultRestoreM :: forall base m t stM stT a
                 . MonadBaseControl base m stM
                => Monad m
                => Monad base
                => MonadTrans t
                => MonadTransControl m t stT
                => base (Compose stM stT a) -> t m a
defaultRestoreM x = restoreT (restoreM (runCompose <$> x))


runCompose :: forall f g a. Compose f g a -> f (g a)
runCompose (Compose x) = x

runIdentity :: forall a. Identity a -> a
runIdentity (Identity x) = x
