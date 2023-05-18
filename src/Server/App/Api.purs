module Server.App.Api
  ( ApiRoute
  , ApiRouterExt
  , ApiRouterCtx
  , ApiRootRoute(..)
  , apiRoute
  , apiRouter
  , JWTPayload
  ) where

import Prelude hiding ((/))

import Conduit.Data.UserId (UserId)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import HTTPurple (type (<+>), Method(..), RouteDuplex', jsonHeaders, noArgs, notFound, ok', sum, (/), (<+>))
import Server.App.Drivers.Articles (ArticlesRoute, articlesRoute, mkArticlesRouter)
import Server.App.Drivers.Profiles (ProfilesRoute, profilesRoute, mkProfilesRouter)
import Server.App.Drivers.User (UserRoute, UserRouterExt, mkUserRouter, userRoute)
import Server.Core.Services.Articles (ArticleService)
import Server.Core.Services.Comment (CommentService)
import Server.Core.Services.User (UserService)
import Server.Infra.HttPurple.Routes ((</>))
import Server.Infra.HttPurple.Types (OmRouter)
import Yoga.JSON (writeJSON)
import Yoga.Om (Om, fromAff, ask)

data ApiRootRoute = Hello

derive instance genericApiRoute :: Generic ApiRootRoute _

type ApiRoute = ArticlesRoute <+> ProfilesRoute <+> UserRoute <+> ApiRootRoute

apiRoute :: RouteDuplex' ApiRoute
apiRoute = articlesRoute <+> profilesRoute <+> userRoute <+> sum
  { "Hello": "hello" / noArgs
  }

apiRootRouter :: forall ext. OmRouter ApiRootRoute ext
apiRootRouter { route: Hello, method: Get } = fromAff $ ok' jsonHeaders $ writeJSON { message: "Hello Api" }
apiRootRouter { route: Hello } = fromAff $ notFound

type JWTPayload = { userId :: UserId, iat :: Int }
type ApiRouterExt ext = UserRouterExt (authed :: Maybe JWTPayload | ext)
type ApiRouterCtx ctx =
  { userService :: UserService
  , articleService :: ArticleService
  , commentService :: CommentService
  | ctx
  }

apiRouter :: forall ctx ext. Om (ApiRouterCtx ctx) () (OmRouter ApiRoute (ApiRouterExt ext))
apiRouter = do
  { userService, articleService, commentService } <- ask
  let
    userRouter = mkUserRouter { userService }
    profilesRouter = mkProfilesRouter { userService }
    articlesRouter = mkArticlesRouter { articleService, commentService, userService }
  pure $ articlesRouter </> profilesRouter </> userRouter </> apiRootRouter
