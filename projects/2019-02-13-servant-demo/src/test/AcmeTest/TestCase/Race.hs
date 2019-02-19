{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AcmeTest.TestCase.Race where

import Test.Tasty
import Test.Tasty.HUnit (testCase, assertEqual)

import Control.Monad.Free (foldFree)
import Control.Concurrent.STM (atomically)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)

import Acme.Model (Counter (..))
import Acme.Effect (runSTMStateEff, StateEff, StateEff' (..))

import Acme.Handler
  ( getCountHandler
  , incrementCountHandler
  )

testConcurrentIncrement
  :: Int      -- number of threads
  -> IO Counter   -- getCount
  -> IO Counter   -- incrementCount
  -> IO ()    -- pass if final count matches
testConcurrentIncrement times getCount incrementCount =
  do
    (Counter initialCount) <- getCount
    _ <- mapConcurrently (\_ -> incrementCount) [1..times]
    (Counter finalCount) <- getCount
    assertEqual "final count should have all count incremented race-free "
      finalCount (initialCount + times)
    return ()

raceTestSTMEff :: Int -> Int -> IO ()
raceTestSTMEff initialCount times = do
  tvar <- liftIO $ atomically $ newTVar $ Counter $ initialCount

  let getCount = atomically $ runSTMStateEff getCountHandler tvar
  let incrementCount = atomically $ runSTMStateEff incrementCountHandler tvar

  testConcurrentIncrement times getCount incrementCount

-- This is the wrong interpretation of the StateEff free monad
-- that is not thread-safe. It lifts each get/put action directly
-- to IO, making each STM operation atomic on its own. The
-- result is that if multiple increment handlers run concurrently,
-- they may increment based on an old counter state that is
-- updated between get and put.
handleIOStateEff :: forall s a. StateEff s a -> TVar s -> IO a
handleIOStateEff eff tvar = do
  foldFree interpSTMStateEff eff
  where
    interpSTMStateEff :: forall x. StateEff' s x -> IO x
    interpSTMStateEff (Get cont) = do
      s <- atomically $ readTVar tvar
      return $ cont s

    interpSTMStateEff (Put s cont) = do
      atomically $ writeTVar tvar s
      return $ cont ()

-- If we run handleIOStateEff with enough concurrent threads,
-- we may get a race condition
raceTestIOStateEff :: Int -> Int -> IO ()
raceTestIOStateEff initialCount times = do
  tvar <- atomically $ newTVar $ Counter initialCount

  let getCount = handleIOStateEff getCountHandler tvar
  let incrementCount = handleIOStateEff incrementCountHandler tvar

  testConcurrentIncrement times getCount incrementCount

raceTests :: TestTree
raceTests = testGroup "Race Condition Tests"
  -- runSTMStateEff should not have race condition no matter how many
  -- concurrent threads running
  [ testCase "Race test under (STMStateEff Counter)" (raceTestSTMEff 0 200000)

  -- Uncomment the test case here to show the failing result
  -- , testCase "Race test under (IO Counter)" (raceTestIOStateEff 0 2000000)
  ]