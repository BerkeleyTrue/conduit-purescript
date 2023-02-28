module Server.Main (main) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Effect.Console (log)
import HTTPurple (Method(..), Request, ResponseM, RouteDuplex', catchAll, mkRoute, noArgs, notFound, ok, response, serve, (/))
import HTTPurple.Status as Status
import Server.Infra.HTTPurple.GracefullShutdown (gracefullShutdown)
import Server.Infra.HttPurple.Middleware.Logger (developmentLogFormat)

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

port :: Int
port = 3000

main :: Effect Unit
main = serve opts settings >>= gracefullShutdown
  where
  onStarted = log $ "Server started on port " <> show port
  opts = { port, onStarted }
  settings = { route, router: developmentLogFormat router }
