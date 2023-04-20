module Server.App.Drivers.User
  ( UserRoute(..)
  , userRoute
  , mkUserRouter
  , UserRouterDeps
  ) where

import Prelude hiding ((/))

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign (MultipleErrors)
import HTTPurple (Method(..), RouteDuplex', badRequest, noArgs, notFound, ok, sum, toString, (/))
import Server.Core.Ports.Ports (UserCreateInput)
import Server.Core.Services.User (UserService(..))
import Server.Infra.HttPurple.Types (Router)
import Yoga.JSON (readJSON)

data UserRoute
  = Register
  | Authen
  | Authed

derive instance genericUserRoute :: Generic UserRoute _

userRoute :: RouteDuplex' UserRoute
userRoute = sum
  { "Authed": "user" / noArgs
  , "Register": "users" / noArgs
  , "Authen": "users" / "login" / noArgs
  }

type UserRouterDeps m =
  { userService :: UserService m
  }

mkUserRouter :: UserRouterDeps Aff -> Router UserRoute
mkUserRouter { userService: (UserService { register }) } { route: Register, method: Post, body } = do
  str <- toString body
  liftEffect $ log str
  let (parsed :: (Either MultipleErrors { user :: UserCreateInput })) = readJSON str
  case parsed of
    Left err -> do
      let parseErr = "Parse error: " <> show err
      liftEffect $ log parseErr
      badRequest parseErr
    Right input -> do
      res <- register input.user
      case res of
        Left err -> do
          liftEffect $ log $ "Error: " <> show err
          badRequest $ show err
        Right _ -> ok "Register success"

mkUserRouter _ { route: Register } = notFound

mkUserRouter _ { route: Authen, method: Post } = ok "authenticate"
mkUserRouter _ { route: Authen } = notFound

mkUserRouter _ { route: Authed, method: Put } = ok "Update user"
mkUserRouter _ { route: Authed, method: Get } = ok "Find user"
mkUserRouter _ { route: Authed } = notFound
