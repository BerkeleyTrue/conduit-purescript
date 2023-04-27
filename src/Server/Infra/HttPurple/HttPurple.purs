module Server.Infra.HttPurple
  ( omServer
  , ServerCtx
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Class (liftEffect)
import Effect.Console (log, error)
import Effect.Exception (message)
import HTTPurple (RouteDuplex', internalServerError, response, serve)
import HTTPurple.Status as Status
import Server.Infra.HttPurple.Middleware.Logger (developmentLogFormat)
import Server.Infra.HttPurple.Server (ExceptionHandler, omToRouter)
import Server.Infra.HttPurple.Types (Router, OmRouter)
import Server.Infra.Node.GracefullShutdown (gracefullShutdown)
import Yoga.Om (Om, ask)

notFoundHandler :: Router Unit
notFoundHandler = const $ response Status.notFound "Could not find the requested resource."

type ServerCtx ctx = { port :: Int | ctx}

defaultOnError :: ExceptionHandler
defaultOnError = \err -> do
  liftEffect $ error $ message err
  internalServerError "Internal server error."

omServer :: forall route ctx. RouteDuplex' route -> OmRouter route -> (Maybe ExceptionHandler) -> Om (ServerCtx ctx) () Unit
omServer route router maybeOnError = do
  { port } <- ask

  let
    onError = fromMaybe defaultOnError maybeOnError
    enhanceRouter = developmentLogFormat <<< omToRouter onError
    onStarted = log $ "Server started on port " <> show port
    opts = { port, onStarted, notFoundHandler: Just notFoundHandler }
    settings = { route, router: enhanceRouter router }

  liftEffect $ serve opts settings >>= gracefullShutdown
