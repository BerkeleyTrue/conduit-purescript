module Server.App.Drivers.User
  ( UserRoute(..)
  , userRoute
  , mkUserRouter
  , UserRouterDeps
  ) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Effect.Class (class MonadEffect)
import HTTPurple (Method(..), RouteDuplex', fromJson, noArgs, notFound, ok, prefix, sum, usingCont, (/))
import HTTPurple.Json.Yoga (jsonDecoder)
import Server.Core.Ports.Ports (UserCreateInput)
import Server.Core.Services.User (UserService(..))
import Server.Infra.HttPurple.Types (Router)

data UserRoute = Authen | User

derive instance genericUserRoute :: Generic UserRoute _

userRoute :: RouteDuplex' UserRoute
userRoute = prefix "user" $ sum
  { "Authen": "login" / noArgs
  , "User": noArgs
  }

type UserRouterDeps m =
  { userService :: UserService m
  }

mkUserRouter
  :: forall m
   . MonadEffect m
  => UserRouterDeps m
  -> Router UserRoute
mkUserRouter _ { route: Authen, method: Post } = ok "authenticate"
mkUserRouter _ { route: Authen } = notFound

mkUserRouter { userService: (UserService { register }) } { route: User, method: Post, body } = usingCont do
  (createUserInput :: UserCreateInput) <- fromJson jsonDecoder body
  ok $ "Create user done with " <> show createUserInput.username

mkUserRouter _ { route: User, method: Put } = ok "Update user"
mkUserRouter _ { route: User, method: Get } = ok "Find user"
mkUserRouter _ { route: User } = notFound
