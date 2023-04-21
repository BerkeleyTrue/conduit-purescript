module Server.Infra.HttPurple
  ( omServer
  , ServerCtx
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Console (log)
import HTTPurple (RouteDuplex', response, serve)
import HTTPurple.Status as Status
import Server.Infra.HttPurple.Middleware.Logger (developmentLogFormat)
import Server.Infra.HttPurple.Types (Router)
import Server.Infra.Node.GracefullShutdown (gracefullShutdown)
import Yoga.Om (Om, ask)

notFoundHandler :: Router Unit
notFoundHandler = const $ response Status.notFound "Could not find the requested resource."

type ServerCtx route =
  { route :: RouteDuplex' route
  , router :: Router route
  , port :: Int
  }

omServer :: forall route. Om (ServerCtx route) () Unit
omServer = do
  { port, route, router } <- ask
  let
    onStarted = log $ "Server started on port " <> show port
    opts = { port, onStarted, notFoundHandler: Just notFoundHandler }
    settings = { route, router: developmentLogFormat router }

  liftEffect $ serve opts settings >>= gracefullShutdown
