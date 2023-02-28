module Server.Main (main) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Effect.Console (log)
import HTTPurple (Request, RouteDuplex', ServerM, ResponseM, mkRoute, noArgs, ok, serve, (/))

data Route
  = Home
  | Ping

derive instance genericRoute :: Generic Route _

route :: RouteDuplex' Route
route = mkRoute
  { "Home": noArgs
  , "Ping": "ping" / noArgs
  }

router :: Request Route -> ResponseM
router { route: Home } = ok "hello world!"
router { route: Ping } = ok "pong"

main :: ServerM
main =
  serve { port: 8000, onStarted } { route, router }
  where
  onStarted = do
    log "Server started"
