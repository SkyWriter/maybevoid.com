{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MyApp.Effect
  ( StateEff
  , STMStateEff
  , runSTMStateEff
  , handleSTMStateEff
  )
where

import Prelude (id, ($))

import Control.Monad (Monad (..))
import Data.Functor (Functor (..))
import Control.Concurrent.STM (STM)
import Control.Monad.Trans.Class (lift)
import Control.Applicative (Applicative)
import Control.Monad.State.Class (MonadState (..))
import Control.Monad.Reader (ReaderT, ask, runReaderT)

import Control.Monad.Free
  ( Free (..)
  , liftF
  , foldFree
  )

import Control.Concurrent.STM.TVar
  ( TVar
  , readTVar
  , writeTVar
  )

newtype STMStateEff s a = STMStateEff
  ( ReaderT (TVar s) STM a )
  deriving (Functor, Applicative, Monad)

data StateEff' s a where
  Get :: (s -> a) -> StateEff' s a
  Put :: s -> (() -> a) -> StateEff' s a
  deriving (Functor)

type StateEff s = Free (StateEff' s)

instance {-# OVERLAPPING #-} MonadState s (STMStateEff s) where
  get = do
    tvar <- STMStateEff ask
    STMStateEff $ lift $ readTVar tvar

  put s = do
    tvar <- STMStateEff ask
    STMStateEff $ lift $ writeTVar tvar s
    return ()

instance {-# OVERLAPPING #-} MonadState s (StateEff s) where
  get = liftF $ Get id
  put s = liftF $ Put s id

runSTMStateEff :: forall s a. STMStateEff s a -> TVar s -> STM a
runSTMStateEff (STMStateEff eff) tvar = runReaderT eff tvar

handleSTMStateEff :: forall s a. StateEff s a -> STMStateEff s a
handleSTMStateEff eff =
  foldFree interpSTMStateEff eff
  where
    interpSTMStateEff :: forall x. StateEff' s x -> STMStateEff s x
    interpSTMStateEff (Get cont) = do
      s <- get
      return $ cont s

    interpSTMStateEff (Put s cont) = do
      put s
      return $ cont ()
