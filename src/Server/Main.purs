module Server.Main (main) where

import Prelude

import Effect (Effect)
import Server.Infra (createApp)

-- TODO: move port into env config

port :: Int
port = 3000

main :: Effect Unit
main = createApp port
