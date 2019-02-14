module MyApp.App (mkApp, Config (..)) where

import Prelude (Int, IO, (.), ($))

import Data.Proxy (Proxy (..))
import Network.Wai (Application)
import Control.Monad (Monad (..))
import Servant.Server (serve, hoistServer)

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM.TVar (newTVar)
import Control.Concurrent.STM (STM, atomically)

import MyApp.Route (AppRoute)
import MyApp.Model (Counter (..))
import MyApp.Server (mkServer, Env (..))

data Config = Config {
  initialCount :: Int
}

routeSpec :: Proxy AppRoute
routeSpec = Proxy

mkApp' :: Config -> STM Application
mkApp' config = do
  tvar <- newTVar $ Counter $ initialCount config
  let env = Env tvar
  let server' = mkServer env
  let server = hoistServer routeSpec (liftIO . atomically) server'
  return $ serve routeSpec server

mkApp :: Config -> IO Application
mkApp = atomically . mkApp'