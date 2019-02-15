{-# LANGUAGE ExplicitForAll #-}

module AcmeTest.Impl.Counter.StateT where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.State.Strict (StateT (..))

import AcmeTest.Util.Effect
import AcmeTest.Abstract.Counter

import Acme.Model (Counter (..))

import Acme.Handler
  ( getCountHandler
  , incrementCountHandler
  )

counterTestStateTIO  :: forall eff.
  ( Monad eff
  , AssertEffect eff
  , MonadIO eff
  ) => Int -> eff ()
counterTestStateTIO initialCount = do
  let finalCount = initialCount + 1

  (s, counter) <- liftIO $ runStateT
    (counterTest
      getCountHandler
      incrementCountHandler
      :: StateT Counter IO Counter)
    (Counter initialCount)

  assertEquals "final state should be 1 + original counter state"
    s (Counter finalCount)
  assertEquals "returned value should be 1 + original counter state"
    counter (Counter finalCount)
