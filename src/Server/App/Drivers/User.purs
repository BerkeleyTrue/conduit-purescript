module Server.App.Drivers.User
  ( UserRoute(..)
  , userRoute
  , mkUserRouter
  , UserRouterDeps
  ) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Effect.Class (class MonadEffect)
import HTTPurple (Method(..), RouteDuplex', fromJson, noArgs, notFound, ok, sum, usingCont, (/))
import HTTPurple.Json.Yoga (jsonDecoder)
import Server.Core.Ports.Ports (UserCreateInput)
import Server.Core.Services.User (UserService(..))
import Server.Infra.HttPurple.Types (Router)

data UserRoute
  = Register
  | Authen
  | User

derive instance genericUserRoute :: Generic UserRoute _

userRoute :: RouteDuplex' UserRoute
userRoute = sum
  { "User": "user" / noArgs
  , "Register": "users" / noArgs
  , "Authen": "users" / "login" / noArgs
  }

type UserRouterDeps m =
  { userService :: UserService m
  }

mkUserRouter
  :: forall m
   . MonadEffect m
  => UserRouterDeps m
  -> Router UserRoute

mkUserRouter { userService: (UserService { register }) } { route: Register, method: Post, body } = usingCont do
  (createUserInput :: UserCreateInput) <- fromJson jsonDecoder body
  ok $ "Create user done with " <> show createUserInput.username
mkUserRouter _ { route: Register } = notFound

mkUserRouter _ { route: Authen, method: Post } = ok "authenticate"
mkUserRouter _ { route: Authen } = notFound

mkUserRouter _ { route: User, method: Put } = ok "Update user"
mkUserRouter _ { route: User, method: Get } = ok "Find user"
mkUserRouter _ { route: User } = notFound
