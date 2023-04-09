module Server.Main (main) where

import Prelude

import Effect.Aff (Aff)
import Server.Infra (createApp)

-- TODO: move port into env config

port :: Int
port = 3000

main :: Aff Unit
main = createApp port
