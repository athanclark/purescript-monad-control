module Main where

import Data.Either (Either (..))
import Data.Functor.Singleton (class SingletonFunctor, liftBaseWith_)
import Control.Monad.Trans.Control (class MonadBaseControl)

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, forkAff, joinFiber, runAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Effect.Console (errorShow, logShow, log)

import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Unsafe.Coerce (unsafeCoerce)
import Queue.One (Queue, new, draw, put, READ, WRITE) as Q


-- | a nonsense type just for the sake of practical application
foreign import data DB :: Type

type Env =
  { host :: String
  , database :: DB
  , outgoing :: Q.Queue (read :: Q.READ, write :: Q.WRITE) String
  }


type AppM = ReaderT Env Aff

-- | Run a server
runAppM :: forall a. Show a => AppM a -> Effect Unit
runAppM x = do
  queue <- Q.new

  let resolve :: forall b. Show b => Either Error b -> Effect Unit
      resolve eX = case eX of
        Left e -> errorShow e
        Right d -> logShow d

  void $ runAff_ resolve $
    runReaderT x
      { host: "localhost"
      , database: unsafeCoerce unit
      , outgoing: queue
      }

  Q.put queue "foo" -- simulating some message being sent


-- | A silly server example
server :: AppM Unit
server = do
  {host,outgoing} <- ask
  liftBaseWith_ \runInBase -> do
    liftEffect (log "bar") -- in Aff
    x <- Q.draw outgoing -- receive some outgoing message...
    -- do something with x here... this is all hypothetical
    thread <- forkAff do
      runInBase do -- in AppM
        {database} <- ask
        -- something clever
        pure unit
    -- something with fiber
    joinFiber thread



-- | A _polymorphic_ server example
doThing :: forall m stM
         . MonadBaseControl Aff m stM
        => MonadEffect m
        => SingletonFunctor stM -- saying "this is basically a functor with one element"
        => m Unit
doThing = do
  liftBaseWith_ \runInBase -> do
    thread <- forkAff do
      runInBase do
        -- in m
        liftEffect (log "woohoo!")
    joinFiber thread





main :: Effect Unit
main = do
  runAppM server
  runAppM doThing -- coercing m to AppM
