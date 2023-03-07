module Server.App.Api
  ( ApiRoute
  , ApiRootRoute(..)
  , apiRoute
  , apiRouter
  ) where

import Prelude hiding ((/))

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import HTTPurple (Method(..), RouteDuplex', noArgs, notFound, ok, orElse, sum, (/), (<+>))
import Server.App.Drivers.Articles (ArticlesRoute, articlesRoute, articlesRouter)
import Server.App.Drivers.Profiles (ProfilesRoute, profilesRoute, profilesRouter)
import Server.App.Drivers.User (UserRoute, userRoute, userRouter)
import Server.Infra.HttPurple.Types (Router)

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

apiRouter :: Router ApiRoute
apiRouter = orElse articlesRouter $ orElse profilesRouter $ orElse userRouter apiRootRouter
