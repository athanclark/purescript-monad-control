module Data.Functor.Singleton where

import Control.Monad.Trans.Control (WriterTStT (..))
import Prelude (class Functor, Unit, unit)
import Data.Functor.Compose (Compose (..))
import Data.Identity (Identity (..))
import Data.Tuple (Tuple (..))


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
