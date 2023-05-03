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

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import HTTPurple (Method(..), RouteDuplex', noArgs, notFound, ok, sum, (/), (<+>), type (<+>))
import Server.App.Drivers.Articles (ArticlesRoute, articlesRoute, articlesRouter)
import Server.App.Drivers.Profiles (ProfilesRoute, profilesRoute, mkProfilesRouter)
import Server.App.Drivers.User (UserRoute, UserRouterExt, mkUserRouter, userRoute)
import Server.Core.Domain.User (UserId)
import Server.Core.Services.User (UserService)
import Server.Infra.HttPurple.Routes ((</>))
import Server.Infra.HttPurple.Types (OmRouter)
import Type.Row (type (+))
import Yoga.Om (Om, fromAff, ask)

data ApiRootRoute = Hello

derive instance genericApiRoute :: Generic ApiRootRoute _

type ApiRoute = ArticlesRoute <+> ProfilesRoute <+> UserRoute <+> ApiRootRoute

apiRoute :: RouteDuplex' ApiRoute
apiRoute = articlesRoute <+> profilesRoute <+> userRoute <+> sum
  { "Hello": "hello" / noArgs
  }

apiRootRouter :: forall ext. OmRouter ApiRootRoute ext
apiRootRouter { route: Hello, method: Get } = fromAff $ ok "Hello Api"
apiRootRouter { route: Hello } = fromAff $ notFound

type JWTPayload = { userId :: UserId, iat:: Int }
type ApiRouterExt ext = UserRouterExt + (authed :: Maybe JWTPayload | ext)
type ApiRouterCtx ctx = { userService :: UserService | ctx }

apiRouter :: forall ctx ext. Om (ApiRouterCtx ctx) () (OmRouter ApiRoute (ApiRouterExt ext))
apiRouter = do
  { userService } <- ask
  let
    userRouter = mkUserRouter { userService: userService }
    profilesRouter = mkProfilesRouter { userService: userService }
  pure $ articlesRouter </> profilesRouter </> userRouter </> apiRootRouter
