{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module MyApp.Route
  ( HelloRoute
  , GetCountRoute
  , IncrementCountRoute
  , ApiRoute
  , AppRoute
  )
where

import Prelude (String)

import Servant.API
  ( (:<|>) (..)
  , (:>)
  , Get
  , Post
  , JSON
  , PlainText
  )

import MyApp.Model (Counter)

type HelloRoute = "hello" :> Get '[PlainText] String
type GetCountRoute = "counter" :> Get '[JSON] Counter
type IncrementCountRoute = "counter" :> "increment" :> Post '[JSON] Counter

type ApiRoute = HelloRoute
  :<|> GetCountRoute
  :<|> IncrementCountRoute

type AppRoute = "api" :> ApiRoute
