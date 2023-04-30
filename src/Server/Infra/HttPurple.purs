module Server.Infra.HttPurple
  ( omServer
  , ServerCtx
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import HTTPurple (RouteDuplex', serve)
import Server.Infra.HttPurple.Types (Router)
import Server.Infra.Node.GracefullShutdown (gracefullShutdown)
import Yoga.Om (Om, ask)

type ServerCtx ctx =
  { port :: Int
  | ctx
  }

type ServerOpts =
  { notFoundHandler :: Router Unit
  , onStarted :: Effect Unit
  }

omServer :: forall route ctx. ServerOpts -> RouteDuplex' route -> Router route -> Om (ServerCtx ctx) () Unit
omServer opts route router = do
  { port } <- ask

  let
    servOpts =
      { port
      , onStarted: opts.onStarted
      , notFoundHandler: opts.notFoundHandler
      }
    settings = { route, router }

  liftEffect $ serve servOpts settings >>= gracefullShutdown
