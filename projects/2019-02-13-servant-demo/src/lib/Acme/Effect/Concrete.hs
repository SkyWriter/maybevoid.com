{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Acme.Effect.Concrete
  ( StateEff
  , STMStateEff
  , runSTMStateEff
  , handleSTMStateEff
  )
where

import Control.Monad (Monad)
import Data.Functor (Functor)
import Control.Concurrent.STM (STM)
import Control.Monad.Free (foldFree)
import Control.Monad.Trans.Class (lift)
import Control.Applicative (Applicative)
import Control.Monad.State.Class (MonadState (..))
import Control.Monad.Reader
  ( ask
  , ReaderT
  , runReaderT
  , MonadReader (..)
  )

import Control.Concurrent.STM.TVar
  ( TVar
  , readTVar
  , writeTVar
  )

import Acme.Effect.Abstract (StateEff, StateEff' (..))

newtype STMStateEff s a = STMStateEff
  ( ReaderT (TVar s) STM a )
  deriving (Functor, Applicative, Monad)

instance {-# OVERLAPPING #-} MonadState s (STMStateEff s) where
  get = do
    tvar <- STMStateEff ask
    STMStateEff $ lift $ readTVar tvar

  put s = do
    tvar <- STMStateEff ask
    STMStateEff $ lift $ writeTVar tvar s
    return ()

runSTMStateEff :: forall s a. STMStateEff s a -> TVar s -> STM a
runSTMStateEff (STMStateEff eff) tvar = runReaderT eff tvar

handleSTMStateEff :: forall s a. StateEff s a -> STMStateEff s a
handleSTMStateEff eff = do
  tvar <- STMStateEff ask
  STMStateEff $ lift $ foldFree (interpSTMStateEff tvar) eff
  where
    interpSTMStateEff :: forall x. TVar s -> StateEff' s x -> STM x
    interpSTMStateEff tvar (Get cont) = do
      s <- readTVar tvar
      return $ cont s

    interpSTMStateEff tvar (Put s cont) = do
      writeTVar tvar s
      return $ cont ()
