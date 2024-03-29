module Server.App.Api
  ( ApiRoute
  , ApiRootRoute(..)
  , ApiRouterCtx
  , ApiRouterExt
  , apiRoute
  , apiRouter
  ) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import HTTPurple (type (<+>), Method(..), RouteDuplex', jsonHeaders, noArgs, notFound, ok', sum, (/), (<+>))
import Server.App.Drivers.Articles (ArticlesRoute, articlesRoute, mkArticlesRouter)
import Server.App.Drivers.Profiles (ProfilesRoute, profilesRoute, mkProfilesRouter)
import Server.App.Drivers.Tags (TagRoute, mkTagsRouter, tagRoute)
import Server.App.Drivers.User (UserRoute, UserRouterExt, mkUsersRouter, userRoute)
import Server.Core.Services.Articles (ArticleService)
import Server.Core.Services.Comment (CommentService)
import Server.Core.Services.Tags (TagService)
import Server.Core.Services.Token (JwtPayload, TokenService)
import Server.Core.Services.User (UserService)
import Server.Infra.HttPurple.Routes ((</>))
import Server.Infra.HttPurple.Types (OmRouter)
import Yoga.JSON (writeJSON)
import Yoga.Om (Om, fromAff, ask)

data ApiRootRoute = Hello

derive instance genericApiRoute :: Generic ApiRootRoute _

type ApiRoute = ArticlesRoute <+> ProfilesRoute <+> UserRoute <+> TagRoute <+> ApiRootRoute

apiRoute :: RouteDuplex' ApiRoute
apiRoute = articlesRoute <+> profilesRoute <+> userRoute <+> tagRoute <+> sum
  { "Hello": "hello" / noArgs
  }

apiRootRouter :: forall ext. OmRouter ApiRootRoute ext
apiRootRouter { route: Hello, method: Get } = fromAff $ ok' jsonHeaders $ writeJSON { message: "Hello Api" }
apiRootRouter { route: Hello } = fromAff $ notFound

type ApiRouterExt ext = UserRouterExt (authed :: Maybe JwtPayload | ext)
type ApiRouterCtx ctx =
  { userService :: UserService
  , articleService :: ArticleService
  , commentService :: CommentService
  , tagService :: TagService
  , tokenService :: TokenService
  | ctx
  }

apiRouter :: forall ctx ext. Om (ApiRouterCtx ctx) () (OmRouter ApiRoute (ApiRouterExt ext))
apiRouter = do
  { userService, articleService, commentService, tagService, tokenService } <- ask
  let
    usersRouter = mkUsersRouter { userService, tokenService }
    profilesRouter = mkProfilesRouter { userService }
    articlesRouter = mkArticlesRouter { articleService, commentService, userService }
    tagsRouter = mkTagsRouter { tagService }
  pure $ articlesRouter </> profilesRouter </> usersRouter </> tagsRouter </> apiRootRouter
