{-# LANGUAGE ExplicitForAll #-}

module AcmeTest.Impl.Counter.STMEff where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar)
import Control.Monad.IO.Class (MonadIO (..))

import AcmeTest.Util.Effect
import AcmeTest.Abstract.Counter

import Acme.Model (Counter (..))
import Acme.Effect (runSTMStateEff)

import Acme.Handler
  ( getCountHandler
  , incrementCountHandler
  )

counterTestSTMEff :: forall eff.
  ( Monad eff
  , AssertEffect eff
  , MonadIO eff
  ) => Int -> eff ()
counterTestSTMEff initialCount = do
  let finalCount = initialCount + 1

  tvar <- liftIO $ atomically $ newTVar $ Counter initialCount

  let getCount = liftIO $ atomically $ runSTMStateEff getCountHandler tvar
  let incrementCount = liftIO $ atomically $ runSTMStateEff incrementCountHandler tvar

  counter <- counterTest getCount incrementCount

  assertEquals "returned value should be 1 + original counter state"
    counter (Counter finalCount)
