{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MyApp.Server (mkServer, Env (..)) where

import Prelude ()

import Control.Monad (Monad)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM (STM, atomically)
import Control.Monad.State.Class (MonadState (..))

import GHC.Generics (Generic)
import Servant.Server (ServerT)
import Servant.API ((:<|>) (..))

import MyApp.Model (Counter (..))

import MyApp.Handler
  ( helloHandler
  , getCountHandler
  , incrementCountHandler
  )

import MyApp.Route
  ( HelloRoute
  , GetCountRoute
  , IncrementCountRoute
  , AppRoute
  )

import MyApp.Effect
  ( STMStateEff
  , handleSTMStateEff
  )

data Env = Env
  { counterState :: TVar Counter
  }

helloServer :: forall eff. Monad eff => ServerT HelloRoute eff
helloServer = helloHandler

getCountServer :: TVar Counter -> ServerT GetCountRoute STM
getCountServer counterTVar = handleSTMStateEff counterTVar getCountHandler

incrementCountServer :: TVar Counter -> ServerT IncrementCountRoute STM
incrementCountServer counterTVar = handleSTMStateEff counterTVar incrementCountHandler

mkServer :: Env -> ServerT AppRoute STM
mkServer env = helloServer
     :<|> getCountServer counterTVar
     :<|> incrementCountServer counterTVar
  where counterTVar = counterState env
