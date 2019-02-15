module AcmeTest.TestCase.Prop where

import Test.Tasty

import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Monadic (monadicIO)

import AcmeTest.Impl.Counter

propTests :: TestTree
propTests = testGroup "Unit Tests"
  [ QC.testProperty "Counter test under (StateT Counter IO)" $
      monadicIO . counterTestStateTIO

  , QC.testProperty "Counter test under (STMStateEff Counter)" $
      monadicIO . counterTestSTMEff

  , QC.testProperty "Counter test under (StateEff Counter)" $
      monadicIO . counterTestStateEff
  ]