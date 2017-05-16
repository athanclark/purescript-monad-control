module Data.Functor.Singleton where

import Control.Monad.Trans.Control (WriterTStT (..))
import Prelude (class Functor, Unit, unit)
import Data.Functor.Compose (Compose (..))
import Data.Identity (Identity (..))
import Data.Tuple (Tuple (..))


class (Functor f) <= SingletonFunctor f where
  -- | `forall x. getSingleton (map (const x) xs) == x`
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
