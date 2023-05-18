module Server.Infra (omApp) where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Class.Console (log)
import HTTPurple (Middleware, response)
import HTTPurple.Status as Status
import Server.App (route, router)
import Server.App.Api (JWTPayload)
import Server.App.Driven.ArticleRepo.MemStore (mkMemoryArticleRepo)
import Server.App.Driven.CommentRepo.MemStore (mkCommentMemoryRepo)
import Server.App.Driven.UserRepo.MemStore (mkMemoryUserRepo)
import Server.Core.Services.Articles (mkArticleService)
import Server.Core.Services.Comment (mkCommentService)
import Server.Core.Services.User (UserOutput, mkUserService)
import Server.Infra.HttPurple (omServer)
import Server.Infra.HttPurple.Middleware.Jwt (mkAuthenticateJwtMiddleware)
import Server.Infra.HttPurple.Middleware.Logger (developmentLogFormat)
import Server.Infra.HttPurple.Server (omEnhanceRouter)
import Server.Infra.HttPurple.Types (OmRouter, Router)
import Server.Infra.Yoga.JWT (Secret)
import Yoga.Om (Om, ask, expandCtx, widenCtx)

type AppCtx = { port :: Int, tokenSecret :: Secret }

notFoundHandler :: Router Unit
notFoundHandler = const $ response Status.notFound "Could not find the requested resource."

omApp :: Om AppCtx () Unit
omApp = do
  { tokenSecret, port } <- ask
  userService <- expandCtx $ mkMemoryUserRepo Map.empty <#> mkUserService
  articleService <- expandCtx $ mkMemoryArticleRepo Map.empty <#> flip mkArticleService userService
  commentService <- expandCtx $ mkCommentMemoryRepo Map.empty <#> flip mkCommentService articleService
  let
    onStarted = log $ "Server started on port " <> show port

    authUserMiddleware :: forall route. Middleware route () (authed :: Maybe JWTPayload, user:: Maybe UserOutput)
    authUserMiddleware = mkAuthenticateJwtMiddleware userService tokenSecret

    enhanceRouter :: forall route. (OmRouter route (authed :: Maybe JWTPayload, user :: Maybe UserOutput)) -> Router route
    enhanceRouter = developmentLogFormat <<< authUserMiddleware <<< omEnhanceRouter Nothing
    opts = { onStarted, notFoundHandler }

  router' <- widenCtx { userService, articleService, commentService } router <#> enhanceRouter
  omServer opts route router'
