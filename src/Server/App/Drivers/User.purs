module Server.App.Drivers.User
  ( UserRoute(..)
  , userRoute
  , mkUserRouter
  , UserRouterDeps
  , UserRouterExt
  ) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Foreign (MultipleErrors)
import HTTPurple (Method(..), Response, RouteDuplex', badRequest', jsonHeaders, noArgs, notFound, ok, ok', sum, toString, (/))
import Server.Core.Domain.User (User)
import Server.Core.Ports.Ports (UserCreateInput)
import Server.Core.Services.User (UserService(..), UserOutput)
import Server.Infra.HttPurple.Types (OmRouter)
import Yoga.JSON (readJSON, writeJSON)
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

type UserRouterExt ext = (user :: Maybe User | ext)

mkUserRouter :: forall ext. UserRouterDeps -> OmRouter UserRoute (UserRouterExt ext)
-- | register a new user
mkUserRouter { userService: (UserService { register }) } { route: Register, method: Post, body } = handleErrors errorHandlers do
  str <- fromAff $ toString body
  parsed <- expandErr $ parseUserFromJson str
  expandErr $ (userToResponse <<< register) parsed.user

  where
  parseUserFromJson :: String -> Om {} (parsingError :: MultipleErrors) { user :: UserCreateInput }
  parseUserFromJson = throwLeftAsM (\err -> throw { parsingError: err }) <<< readJSON

  userToResponse :: forall errs. Om {} (userRepoErr :: String | errs) UserOutput -> Om {} (userRepoErr :: String | errs) Response
  userToResponse userOm = userOm >>= ok' jsonHeaders <<< writeJSON

  errorHandlers =
    { userRepoErr: \err -> badRequest' jsonHeaders $ writeJSON { message: show err }
    , parsingError: \err -> badRequest' jsonHeaders $ writeJSON { message: show err }
    }

mkUserRouter _ { route: Register } = notFound

-- | login a user
mkUserRouter _ { route: Authen, method: Post } = ok "authenticate"
mkUserRouter _ { route: Authen } = notFound

-- | update the current user
mkUserRouter _ { route: Authed, method: Put } = ok "Update user"
mkUserRouter _ { route: Authed, method: Get } = ok "Find user"
mkUserRouter _ { route: Authed } = notFound
