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
import Server.App.Drivers.User (UserRouterExt)
import Server.Core.Domain.User (User)
import Server.Infra.HttPurple.Middleware.Jwt (mkAuthenticateUserMiddleware)
import Server.Infra.HttPurple.Middleware.Logger (developmentLogFormat)
import Server.Infra.HttPurple.Server (ExceptionHandler, omEnhanceRouter)
import Server.Infra.HttPurple.Types (OmRouter, Router)
import Server.Infra.Node.GracefullShutdown (gracefullShutdown)
import Yoga.Om (Om, ask)

notFoundHandler :: Router Unit
notFoundHandler = const $ response Status.notFound "Could not find the requested resource."

type ServerCtx ctx =
  { port :: Int
  , tokenSecret :: String
  | ctx
  }

defaultOnError :: ExceptionHandler
defaultOnError = \err -> do
  liftEffect $ error $ message err
  internalServerError "Internal server error."

omServer :: forall route ctx. RouteDuplex' route -> OmRouter route (UserRouterExt ()) -> (Maybe ExceptionHandler) -> Om (ServerCtx ctx) () Unit
omServer route router maybeOnError = do
  { port, tokenSecret } <- ask

  let
    onError = fromMaybe defaultOnError maybeOnError
    authUserMiddleware = mkAuthenticateUserMiddleware tokenSecret

    enhanceRouter :: (OmRouter route (user :: Maybe User)) -> Router route
    enhanceRouter = authUserMiddleware <<< developmentLogFormat <<< omEnhanceRouter onError

    onStarted = log $ "Server started on port " <> show port
    opts = { port, onStarted, notFoundHandler: Just notFoundHandler }
    settings = { route, router: enhanceRouter router }

  liftEffect $ serve opts settings >>= gracefullShutdown
