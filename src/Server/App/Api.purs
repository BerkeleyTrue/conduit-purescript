module Server.App.Api (ApiRoute(..), apiRoute, apiRouter) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import HTTPurple (Method(..), Request, ResponseM, RouteDuplex', noArgs, notFound, ok, sum, (/))

data ApiRoute = Hello

derive instance genericApiRoute :: Generic ApiRoute _

apiRoute :: RouteDuplex' ApiRoute
apiRoute = sum
  { "Hello": "hello" / noArgs
  }

apiRouter :: Request ApiRoute -> ResponseM
apiRouter { route: Hello, method: Get } = ok "Hello Api"
apiRouter { route: Hello } = notFound
