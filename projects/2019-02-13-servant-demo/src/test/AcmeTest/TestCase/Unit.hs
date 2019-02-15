module AcmeTest.TestCase.Unit where

import Test.Tasty

import Test.Tasty.HUnit as HU (testCase)
import AcmeTest.Impl.Counter

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "Counter test under (StateT Counter IO)" (counterTestStateTIO 0)
  , testCase "Counter test under (STMStateEff Counter)" (counterTestSTMEff 0)
  , testCase "Counter test under (StateEff Counter)" (counterTestStateEff 0)
  ]