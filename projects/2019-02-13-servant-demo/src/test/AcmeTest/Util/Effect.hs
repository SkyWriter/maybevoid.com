{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module AcmeTest.Util.Effect where

import Test.QuickCheck.Monadic (PropertyM (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Test.Tasty.HUnit as HU (assertEqual, assertBool)

import qualified Test.QuickCheck.Monadic as QCM (assert)

class Monad eff => AssertEffect eff where
  assert :: String -> Bool -> eff ()

  assertEquals :: forall a. (Eq a, Show a) => String -> a -> a -> eff ()
  assertEquals msg x y = assert msg (x == y)

instance AssertEffect IO where
  assert = assertBool
  assertEquals = assertEqual

instance {-# OVERLAPPING #-} (Monad eff) => AssertEffect (PropertyM eff) where
  assert _ = QCM.assert

instance
  ( Monad eff
  , Monad (t eff)
  , AssertEffect eff
  , MonadTrans t
  ) => AssertEffect (t eff) where
    assert msg b = lift $ assert msg b
    assertEquals msg x y = lift $ assertEquals msg x y
