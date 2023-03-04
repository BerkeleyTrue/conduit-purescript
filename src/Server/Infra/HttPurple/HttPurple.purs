module Server.Infra.HttPurple (createServer) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import HTTPurple (Request, ResponseM, RouteDuplex', response, serve)
import HTTPurple.Status as Status
import Server.Infra.HttPurple.Middleware.Logger (developmentLogFormat)
import Server.Infra.Node.GracefullShutdown (gracefullShutdown)


notFoundHandler :: Request Unit -> ResponseM
notFoundHandler = const $ response Status.notFound "Oops, something went wrong"

createServer
  :: forall route
   . RouteDuplex' route
  -> (Request route -> ResponseM)
  -> Int
  -> Effect Unit
createServer route router port = serve opts settings >>= gracefullShutdown
  where
  onStarted = log $ "Server started on port " <> show port
  opts = { port, onStarted, notFoundHandler: Just notFoundHandler }
  settings = { route, router: developmentLogFormat router }
