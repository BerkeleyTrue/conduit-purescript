module Server.App.Drivers.User
  ( UserRoute(..)
  , userRoute
  , mkUserRouter
  , UserRouterDeps
  ) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Foreign (MultipleErrors)
import HTTPurple (Method(..), Response, RouteDuplex', badRequest, noArgs, notFound, ok, sum, toString, (/))
import Server.Core.Ports.Ports (UserCreateInput)
import Server.Core.Services.User (UserService(..), UserOutput)
import Server.Infra.HttPurple.Types (OmRouter)
import Yoga.JSON (readJSON)
import Yoga.Om (Om, expandErr, fromAff, handleErrors, throw, throwLeftAsM)

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

type UserRouterDeps =
  { userService :: UserService
  }

mkUserRouter :: UserRouterDeps -> OmRouter UserRoute
mkUserRouter { userService: (UserService { register }) } { route: Register, method: Post, body } = handleErrors errorHandlers do
  str <- fromAff $ toString body
  parsed <- expandErr $ parseUserFromJson str
  expandErr $ (userToResponse <<< register) parsed.user

  where
  parseUserFromJson :: String -> Om {} (parsingError :: MultipleErrors) { user :: UserCreateInput }
  parseUserFromJson = throwLeftAsM (\err -> throw { parsingError: err }) <<< readJSON

  userToResponse :: forall errs. Om {} (userRepoErr :: String | errs) UserOutput -> Om {} (userRepoErr :: String | errs) Response
  userToResponse userOm = userOm >>= \user -> ok $ show user

  errorHandlers =
    { userRepoErr: \err -> badRequest $ show err
    , parsingError: \err -> badRequest $ show err
    }

mkUserRouter _ { route: Register } = notFound

mkUserRouter _ { route: Authen, method: Post } = ok "authenticate"
mkUserRouter _ { route: Authen } = notFound

mkUserRouter _ { route: Authed, method: Put } = ok "Update user"
mkUserRouter _ { route: Authed, method: Get } = ok "Find user"
mkUserRouter _ { route: Authed } = notFound
