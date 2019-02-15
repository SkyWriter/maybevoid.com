module Main where

import Network.Wai.Handler.Warp (run)

import Acme.App (mkApp, Config (..))

config :: Config
config = Config {
  initialCount = 5
}

main :: IO ()
main = do
  app <- mkApp config
  run 8081 app