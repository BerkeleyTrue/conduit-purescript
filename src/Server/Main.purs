module Server.Main (main) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Effect.Console (log)
import HTTPurple (Method(..), Request, ResponseM, RouteDuplex', ServerM, catchAll, mkRoute, noArgs, notFound, ok, response, serve, (/))
import HTTPurple.Status as Status

data Route
  = Home
  | Ping
  | CatchAll (Array String)

derive instance genericRoute :: Generic Route _

route :: RouteDuplex' Route
route = mkRoute
  { "Home": noArgs
  , "Ping": "ping" / noArgs
  , "CatchAll": catchAll
  }

router :: Request Route -> ResponseM
router { route: Home, method: Get } = ok "hello world!"
router { route: Ping, method: Get } = ok "pong"
router { route: CatchAll _ } = response Status.notFound "Opps, something went wrong!"
router _ = notFound

main :: ServerM
main =
  serve { port: 8000, onStarted } { route, router }
  where
  onStarted = do
    log "Server started"
