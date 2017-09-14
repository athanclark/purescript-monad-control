module Data.Functor.Singleton where

import Control.Monad.Trans.Control (WriterTStT (..), FreeTStT (..), class MonadBaseControl, liftBaseWith, class MonadTransControl, liftWith)
import Prelude (class Functor, class Monad, Unit, unit, map, (<<<))
import Data.Functor.Compose (Compose (..))
import Data.Identity (Identity (..))
import Data.Tuple (Tuple (..))
import Data.Either (Either (..))
import Control.Monad.Free.Trans (resume)
import Control.Monad.Rec.Class (class MonadRec)


-- | Instances must follow these laws:
-- |
-- | ```purescript
-- | inverse :: forall f a. Applicative f => Prop
-- | inverse =
-- |      (getSingleton :: f a -> a) <<< (pure :: a -> f a)
-- |   == (id :: a -> a)
-- |
-- | viaconst :: forall f a b. Functor f => Prop
-- | viaconst = forall (x :: b). forall (xs :: f a).
-- |      (getSingleton :: f b -> b) <<< (map (const x) :: f a -> f b)
-- |   == (const x :: f a -> b)
-- | ```
class (Functor f) <= SingletonFunctor f where
  getSingleton :: forall a. f a -> a


instance singletonFunctorIdentity :: SingletonFunctor Identity where
  getSingleton (Identity x) = x

instance singletonFunctorCompose :: (SingletonFunctor f, SingletonFunctor g) => SingletonFunctor (Compose f g) where
  getSingleton (Compose x) = getSingleton (getSingleton x)

instance singletonFunctorTuple :: SingletonFunctor (Tuple a) where
  getSingleton (Tuple _ x) = x

instance singletonFunctorUnitFunction :: SingletonFunctor ((->) Unit) where
  getSingleton f = f unit

instance singletonFunctorWriterTStT :: SingletonFunctor (WriterTStT w) where
  getSingleton (WriterTStT _ x) = x

instance singletonFunctorFreeTStT :: (SingletonFunctor f, SingletonFunctor m, MonadRec m) => SingletonFunctor (FreeTStT f m) where
  getSingleton (FreeTStT e) = case e of
    Left x -> x
    Right x -> getSingleton (FreeTStT (getSingleton (resume (getSingleton x))))


liftWith_ :: forall t m stT b
           . MonadTransControl m t stT
          => SingletonFunctor stT
          => Monad m
          => ((forall a. t m a -> m a) -> m b) -> t m b
liftWith_ f = liftWith \run -> f (map getSingleton <<< run)

liftBaseWith_ :: forall base m stM b
               . MonadBaseControl base m stM
               => SingletonFunctor stM
               => Functor base
               => ((forall a. m a -> base a) -> base b)
               -> m b
liftBaseWith_ f = liftBaseWith \runInBase -> f (map getSingleton <<< runInBase)
