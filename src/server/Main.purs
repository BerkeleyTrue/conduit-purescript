module Server.Main (main) where

import Prelude hiding ((/))

import Effect.Console (log)
import Data.Generic.Rep (class Generic)
import HTTPurple (ServerM, ok, serve)
import Routing.Duplex as RD
import Routing.Duplex.Generic as RG

data Route = SayHello

derive instance Generic Route _

route :: RD.RouteDuplex' Route
route = RD.root $ RG.sum
  { "SayHello": RG.noArgs
  }

main :: ServerM
main =
  serve { port: 8000, onStarted } { route, router: const $ ok "hello world!" }
  where
  onStarted = do
    log "Server started"
