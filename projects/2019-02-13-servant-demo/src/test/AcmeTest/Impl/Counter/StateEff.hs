{-# LANGUAGE ExplicitForAll #-}

module AcmeTest.Impl.Counter.StateEff where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar)
import Control.Monad.IO.Class (MonadIO (..))

import AcmeTest.Util.Effect
import AcmeTest.Abstract.Counter

import Acme.Model (Counter (..))
import Acme.Effect (runSTMStateEff, handleSTMStateEff)

import Acme.Handler
  ( getCountHandler
  , incrementCountHandler
  )

counterTestStateEff :: forall eff.
  ( Monad eff
  , AssertEffect eff
  , MonadIO eff
  ) => Int -> eff ()
counterTestStateEff initialCount = do
  let finalCount = initialCount + 1

  tvar <- liftIO $ atomically $ newTVar $ Counter initialCount

  let getCount = liftIO $ atomically $ runSTMStateEff
          (handleSTMStateEff getCountHandler) tvar
  let incrementCount = liftIO $ atomically $ runSTMStateEff
          (handleSTMStateEff incrementCountHandler) tvar

  counter <- counterTest getCount incrementCount

  assertEquals "returned value should be 1 + original counter state"
    counter (Counter finalCount)
