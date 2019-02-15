{-# LANGUAGE ExplicitForAll #-}

module AcmeTest.Abstract.Counter (counterTest) where

import Acme.Model (Counter (..))
import AcmeTest.Util.Effect (AssertEffect (..))

counterTest :: forall eff
  . (Monad eff, AssertEffect eff)
  => eff Counter
  -> eff Counter
  -> eff Counter
counterTest getCount incrementCount = do
  counter1 <- getCount
  counter2 <- getCount
  assertEquals "getCount should return same counter state"
    counter1 counter2

  counter3 <- incrementCount
  assertEquals "incrementCountHandler should increment and return counter state"
    ((count counter1) + 1) (count counter3)

  counter4 <- getCount
  assertEquals "getCountHandler should return new counter state"
    counter3 counter4

  return counter4
