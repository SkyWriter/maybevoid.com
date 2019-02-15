{-# LANGUAGE DeriveGeneric #-}

module Acme.Model (Counter (..)) where

import Prelude (Int, Eq, Show)

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

data Counter = Counter
  { count :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON Counter
