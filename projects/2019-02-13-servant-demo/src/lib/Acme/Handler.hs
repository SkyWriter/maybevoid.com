{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}

module Acme.Handler
  ( helloHandler
  , getCountHandler
  , incrementCountHandler
  )
where

import Prelude (String, ($), (+))

import Control.Monad (Monad (..))
import Control.Monad.State.Class (MonadState (..))

import Acme.Model (Counter (..))

helloHandler :: forall eff. Monad eff => eff String
helloHandler = return "Hello World"

getCountHandler :: forall eff
  . (MonadState Counter eff)
  => eff Counter
getCountHandler = get

incrementCountHandler :: forall eff
  . (MonadState Counter eff)
  => eff Counter
incrementCountHandler = do
  oldCounter <- get
  let newCounter = Counter $ count oldCounter + 1
  put newCounter
  return newCounter
