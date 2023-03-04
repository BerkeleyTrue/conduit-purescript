module Server.App.Api (ApiRoute, ApiRootRoute(..), apiRoute, apiRouter) where

import Prelude hiding ((/))

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import HTTPurple (Method(..), Request, ResponseM, RouteDuplex', noArgs, notFound, ok, orElse, sum, (/), (<+>))
import Server.App.Api.Profiles (ProfilesRoute, profilesRoute, profilesRouter)
import Server.App.Api.User (UserRoute, userRoute, userRouter)

data ApiRootRoute = Hello

derive instance genericApiRoute :: Generic ApiRootRoute _

type ApiRoute = Either ProfilesRoute (Either UserRoute ApiRootRoute)

apiRoute :: RouteDuplex' ApiRoute
apiRoute = profilesRoute <+> userRoute <+> sum
  { "Hello": "hello" / noArgs
  }

apiRootRouter :: Request ApiRootRoute -> ResponseM
apiRootRouter { route: Hello, method: Get } = ok "Hello Api"
apiRootRouter { route: Hello } = notFound

apiRouter :: Request ApiRoute -> ResponseM
apiRouter = profilesRouter `orElse` (userRouter `orElse` apiRootRouter)
