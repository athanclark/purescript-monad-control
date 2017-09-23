module Main where

import Data.Tuple
import Data.Functor.Singleton (getSingleton)
import Control.Monad.Base
import Control.Monad.Trans.Control

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console

import Control.Monad.Reader
import Control.Monad.Writer
import Unsafe.Coerce
import Queue


type Env =
  { host :: String
  , database :: DB
  , outgoing :: Queue String
  }


type AppM eff = ReaderT Env (Aff eff)

runAppM :: forall a eff. AppM eff a -> Eff eff Unit
runAppM x = do
  queue <- newQueue
  void $ runAff errorShow logShow $
    runReaderT x {host: "localhost", database: unsafeCoerce unit, outgoing: queue}



server :: AppM _ Unit
server = do
  {host,outgoing} <- ask
  liftBaseWith_ \runner -> do
    liftEff $ log "bar"
    liftEff $ onQueue queue $ \x ->
      database.blah...
    f <- forkAff $ do
      runner $ do
        {database} <- ask
        -- something clever
        x <- readQueue queue
        pure unit
    -- something with fiber
    joinFiber f


StT (WriterT) = Tuple a ...
              = Tuple ... a

x :: MonadBaseControl (Eff eff) m stT =>
  => SingletonFunctor (stT m) => m Unit
x = do
  liftBaseWith_ \runner ->
    thread <- async $ do
      runner (y :: AppM _ Unit)







main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  runFooM x
