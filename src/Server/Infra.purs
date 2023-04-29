module Server.Infra (omApp) where

import Prelude hiding ((/))

import Data.Maybe (Maybe(..))
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

omApp :: Om AppCtx () Unit
omApp = do
  { tokenSecret } <- ask
  let
    authUserMiddleware = mkAuthenticateUserMiddleware tokenSecret
    enhanceRouter :: forall route. (OmRouter route (user :: Maybe User)) -> Router route
    enhanceRouter = authUserMiddleware <<< developmentLogFormat <<< omEnhanceRouter Nothing

  router' <- router <#> enhanceRouter
  omServer route router'
