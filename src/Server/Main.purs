module Server.Main (main) where

import Prelude

import Effect (Effect)
import Server.Infra.CreateApp (createApp)

port :: Int
port = 3000

main :: Effect Unit
main = createApp port
