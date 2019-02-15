{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Acme.Effect.Abstract
  ( StateEff
  , StateEff' (..)
  )
where

import Prelude (id, ($))

import Data.Functor (Functor (..))
import Control.Monad.State.Class (MonadState (..))

import Control.Monad.Free
  ( Free (..)
  , liftF
  )

data StateEff' s a where
  Get :: (s -> a) -> StateEff' s a
  Put :: s -> (() -> a) -> StateEff' s a
  deriving (Functor)

type StateEff s = Free (StateEff' s)

instance {-# OVERLAPPING #-} MonadState s (StateEff s) where
  get = liftF $ Get id
  put s = liftF $ Put s id
