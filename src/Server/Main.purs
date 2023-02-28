module Server.Main (main) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Posix.Signal (Signal(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import HTTPurple (Method(..), Request, ResponseM, RouteDuplex', catchAll, mkRoute, noArgs, notFound, ok, response, serve, (/))
import HTTPurple.Status as Status
import Node.Process (onSignal)
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
router { route: Home, method: Get } = ok "hello foo!"
router { route: Ping, method: Get } = ok "pong"
router { route: CatchAll _ } = response Status.notFound "Opps, something went wrong!"
router _ = notFound

port :: Int
port = 3000

main :: Effect Unit
main = do
  liftEffect do
    shutdown <- serve { port, onStarted } { route, router: developmentLogFormat router }

    let shutdownHandler = launchAff_ do
          liftEffect $ log "Shutting down server..."
          liftEffect $ shutdown $ log "Server shutdown"

    onSignal SIGINT shutdownHandler
    onSignal SIGTERM shutdownHandler
    onSignal SIGUSR2 shutdownHandler -- nodemon

  where
  onStarted = log $ "Server started on port " <> show port
