module Server.Infra.HttPurple (createServer) where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import HTTPurple (Request, ResponseM, RouteDuplex', serve)
import Server.Infra.HttPurple.Middleware.Logger (developmentLogFormat)
import Server.Infra.Node.GracefullShutdown (gracefullShutdown)

createServer
  :: forall route
   . RouteDuplex' route
  -> (Request route -> ResponseM)
  -> Int
  -> Effect Unit
createServer route router port = serve opts settings >>= gracefullShutdown
  where
  onStarted = log $ "Server started on port " <> show port
  opts = { port, onStarted }
  settings = { route, router: developmentLogFormat router }
