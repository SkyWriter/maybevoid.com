{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.State.Strict (StateT (..))

import MyApp.Model (Counter (..))
import MyApp.Effect (runSTMStateEff, handleSTMStateEff)

import MyApp.Handler
  ( getCountHandler
  , incrementCountHandler
  )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Demo App Tests" [unitTests]

counterTest :: forall eff
  . (MonadIO eff)
  => eff Counter
  -> eff Counter
  -> eff Counter
counterTest getCount incrementCount = do
  counter1 <- getCount
  counter2 <- getCount
  liftIO $ assertEqual "getCount should return same counter state"
    counter1 counter2

  counter3 <- incrementCount
  liftIO $ assertEqual "incrementCountHandler should increment and return counter state"
    ((count counter1) + 1) (count counter3)

  counter4 <- getCount
  liftIO $ assertEqual "getCountHandler should return new counter state"
    counter3 counter4

  return counter4

counterTestStateTIO :: Assertion
counterTestStateTIO = do
  let initialCount = 5
  let finalCount = initialCount + 1
  (s, counter) <- runStateT
    (counterTest
      getCountHandler
      incrementCountHandler
      :: StateT Counter IO Counter)
    (Counter initialCount)
  assertEqual "final state should be 1 + original counter state"
    s (Counter finalCount)
  assertEqual "returned value should be 1 + original counter state"
    counter (Counter finalCount)

counterTestSTMEff :: Assertion
counterTestSTMEff = do
  let initialCount = 8
  let finalCount = initialCount + 1

  tvar <- atomically $ newTVar $ Counter $ initialCount

  let getCount = atomically $ runSTMStateEff getCountHandler tvar
  let incrementCount = atomically $ runSTMStateEff incrementCountHandler tvar

  counter <- counterTest getCount incrementCount

  assertEqual "returned value should be 1 + original counter state"
    counter (Counter finalCount)

counterTestStateEff :: Assertion
counterTestStateEff = do
  let initialCount = 8
  let finalCount = initialCount + 1

  tvar <- atomically $ newTVar $ Counter $ initialCount

  let getCount =
        atomically $ runSTMStateEff
          (handleSTMStateEff getCountHandler) tvar
  let incrementCount =
        atomically $ runSTMStateEff
          (handleSTMStateEff incrementCountHandler) tvar

  counter <- counterTest getCount incrementCount

  assertEqual "returned value should be 1 + original counter state"
    counter (Counter finalCount)

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "Counter test under (StateT Counter IO)" counterTestStateTIO
  , testCase "Counter test under (STMStateEff Counter)" counterTestSTMEff
  , testCase "Counter test under (StateEff Counter)" counterTestStateEff
  ]