module Server.Infra (omApp) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class.Console (log)
import HTTPurple (response)
import HTTPurple.Status as Status
import Server.App (route, router)
import Server.Core.Domain.User (User)
import Server.Infra.HttPurple (omServer)
import Server.Infra.HttPurple.Middleware.Jwt (mkAuthenticateUserMiddleware)
import Server.Infra.HttPurple.Middleware.Logger (developmentLogFormat)
import Server.Infra.HttPurple.Server (omEnhanceRouter)
import Server.Infra.HttPurple.Types (OmRouter, Router)
import Server.Infra.Yoga.JWT (Secret)
import Yoga.Om (Om, ask)

type AppCtx = { port :: Int, tokenSecret :: Secret }

notFoundHandler :: Router Unit
notFoundHandler = const $ response Status.notFound "Could not find the requested resource."

omApp :: Om AppCtx () Unit
omApp = do
  { tokenSecret, port } <- ask
  let
    onStarted = log $ "Server started on port " <> show port

    authUserMiddleware = mkAuthenticateUserMiddleware tokenSecret

    enhanceRouter :: forall route. (OmRouter route (user :: Maybe User)) -> Router route
    enhanceRouter = authUserMiddleware <<< developmentLogFormat <<< omEnhanceRouter Nothing
    opts = { onStarted, notFoundHandler }

  router' <- router <#> enhanceRouter
  omServer opts route router'
