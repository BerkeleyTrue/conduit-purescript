module Server.App.Api
  ( ApiRoute
  , ApiRouterExt
  , ApiRootRoute(..)
  , apiRoute
  , apiRouter
  ) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Map as Map
import HTTPurple (Method(..), RouteDuplex', noArgs, notFound, ok, sum, (/), (<+>), type (<+>))
import Server.App.Driven.UserRepo.MemStore (mkMemoryUserRepo)
import Server.App.Drivers.Articles (ArticlesRoute, articlesRoute, articlesRouter)
import Server.App.Drivers.Profiles (ProfilesRoute, profilesRoute, mkProfilesRouter)
import Server.App.Drivers.User (UserRoute, UserRouterExt, mkUserRouter, userRoute)
import Server.Core.Services.User (mkUserService)
import Server.Infra.HttPurple.Routes ((</>))
import Server.Infra.HttPurple.Types (OmRouter)
import Yoga.Om (Om, fromAff)

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

type ApiRouterExt ext = UserRouterExt ext

apiRouter :: forall ext. Om {} () (OmRouter ApiRoute (ApiRouterExt ext))
apiRouter = do
  userRepo <- mkMemoryUserRepo Map.empty
  userService <- mkUserService userRepo
  let
    userRouter = mkUserRouter { userService: userService }
    profilesRouter = mkProfilesRouter { userService: userService }
  pure $ articlesRouter </> profilesRouter </> userRouter </> apiRootRouter
