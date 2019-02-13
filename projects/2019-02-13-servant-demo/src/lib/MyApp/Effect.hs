{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MyApp.Effect
  ( STMStateEff
  , handleSTMStateEff
  )
where

import Prelude (id, ($))

import Control.Monad (Monad (..))
import Data.Functor (Functor (..))
import Control.Concurrent.STM (STM, atomically)
import Control.Monad.State.Class (MonadState (..))

import Control.Monad.Free
  ( Free (..)
  , liftF
  , foldFree
  )

import Control.Concurrent.STM.TVar
  ( TVar
  , newTVar
  , readTVar
  , writeTVar
  , readTVarIO
  )

data STMStateEff' s a where
  Get :: (s -> a) -> STMStateEff' s a
  Put :: s -> a -> STMStateEff' s a
  deriving (Functor)

type STMStateEff s = Free (STMStateEff' s)

handleSTMStateEff :: forall s a. TVar s -> STMStateEff s a -> STM a
handleSTMStateEff tvar eff = foldFree interp eff
  where
    interp :: forall x. STMStateEff' s x -> STM x
    interp (Get cont) = do
      state <- readTVar tvar
      return $ cont state

    interp (Put state val) = do
      writeTVar tvar state
      return val

instance {-# OVERLAPPING #-} MonadState s (STMStateEff s) where
  get = liftF $ Get id
  put state = liftF $ Put state ()
