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
import Server.Infra.HttPurple.Types (Router)
import Server.Infra.Node.GracefullShutdown (gracefullShutdown)
import Yoga.Om (Om, ask)

notFoundHandler :: Router Unit
notFoundHandler = const $ response Status.notFound "Could not find the requested resource."

type ServerCtx ctx =
  { port :: Int
  | ctx
  }

omServer :: forall route ctx. RouteDuplex' route -> Router route -> Om (ServerCtx ctx) () Unit
omServer route router = do
  { port } <- ask

  let
    onStarted = log $ "Server started on port " <> show port
    opts = { port, onStarted, notFoundHandler: Just notFoundHandler }
    settings = { route, router }

  liftEffect $ serve opts settings >>= gracefullShutdown
