module Server.App.Api
  ( ApiRoute
  , ApiRootRoute(..)
  , apiRoute
  , apiRouter
  ) where

import Prelude hiding ((/))

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import HTTPurple (Method(..), RouteDuplex', noArgs, notFound, ok, orElse, sum, (/), (<+>))
import Server.App.Driven.UserRepo.MemStore (mkMemoryUserRepo)
import Server.App.Drivers.Articles (ArticlesRoute, articlesRoute, articlesRouter)
import Server.App.Drivers.Profiles (ProfilesRoute, profilesRoute, profilesRouter)
import Server.App.Drivers.User (UserRoute, mkUserRouter, userRoute)
import Server.Core.Services.User (mkUserService)
import Server.Infra.HttPurple.Types (Router)
import Yoga.Om (Om)

data ApiRootRoute = Hello

derive instance genericApiRoute :: Generic ApiRootRoute _

type ApiRoute = Either ArticlesRoute (Either ProfilesRoute (Either UserRoute ApiRootRoute))

apiRoute :: RouteDuplex' ApiRoute
apiRoute = articlesRoute <+> profilesRoute <+> userRoute <+> sum
  { "Hello": "hello" / noArgs
  }

apiRootRouter :: Router ApiRootRoute
apiRootRouter { route: Hello, method: Get } = ok "Hello Api"
apiRootRouter { route: Hello } = notFound

apiRouter :: forall ctx. Om { | ctx } () (Router ApiRoute)
apiRouter = do
  userRepo <- mkMemoryUserRepo Map.empty
  let
    userService = mkUserService userRepo
    userRouter = mkUserRouter { userService: userService }
  pure (orElse articlesRouter $ orElse profilesRouter $ orElse userRouter apiRootRouter)
