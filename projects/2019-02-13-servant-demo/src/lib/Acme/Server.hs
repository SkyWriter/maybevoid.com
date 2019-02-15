{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Acme.Server (mkServer, Env (..)) where

import Prelude ()

import Control.Monad (Monad)
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar)

import Servant.Server (ServerT)
import Servant.API ((:<|>) (..))

import Acme.Model (Counter (..))
import Acme.Effect (runSTMStateEff)

import Acme.Handler
  ( helloHandler
  , getCountHandler
  , incrementCountHandler
  )

import Acme.Route
  ( HelloRoute
  , GetCountRoute
  , IncrementCountRoute
  , AppRoute
  )

data Env = Env
  { counterState :: TVar Counter
  }

helloServer :: forall eff. Monad eff => ServerT HelloRoute eff
helloServer = helloHandler

getCountServer :: TVar Counter -> ServerT GetCountRoute STM
getCountServer = runSTMStateEff getCountHandler

incrementCountServer :: TVar Counter -> ServerT IncrementCountRoute STM
incrementCountServer = runSTMStateEff incrementCountHandler

mkServer :: Env -> ServerT AppRoute STM
mkServer env = helloServer
     :<|> getCountServer counterTVar
     :<|> incrementCountServer counterTVar
  where counterTVar = counterState env
