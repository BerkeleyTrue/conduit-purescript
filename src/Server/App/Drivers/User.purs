module Server.App.Drivers.User
  ( UserRoute(..)
  , userRoute
  , mkUsersRouter
  , UserRouterDeps
  , UserRouterExt
  ) where

import Prelude hiding ((/))

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Foreign (MultipleErrors)
import HTTPurple (Method(..), Response, RouteDuplex', badRequest', forbidden, jsonHeaders, noArgs, notFound, ok', sum, toString, (/))
import Server.Core.Ports.Ports (UserCreateInput)
import Server.Core.Services.Token (TokenService(..))
import Server.Core.Services.User (UserLoginInput, UserOutput, UserService(..), UpdateUserInput)
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
  , tokenService :: TokenService
  }

type UserRouterExt ext = (user :: Maybe UserOutput | ext)
type ErrorHandler error = error -> Om {} (userRepoErr :: String) Response

defaultErrorHandlers
  :: forall ctx errOut
   . Om ctx (userRepoErr :: String, parsingError :: MultipleErrors | errOut) Response
  -> Om ctx errOut Response
defaultErrorHandlers = handleErrors
  { userRepoErr: \err -> badRequest' jsonHeaders $ writeJSON { message: show err }
  , parsingError: \err -> badRequest' jsonHeaders $ writeJSON { message: show err }
  }

userToResponse :: forall errs. UserOutput -> Om {} (userRepoErr :: String | errs) Response
userToResponse user = ok' jsonHeaders $ writeJSON ({ user } :: { user :: UserOutput })

mkUsersRouter :: forall ext. UserRouterDeps -> OmRouter UserRoute (UserRouterExt ext)
-- | register a new user
-- NOTE: does not check if currently logged in.
-- NOTE: updates token
mkUsersRouter { userService: (UserService { register }), tokenService: (TokenService { updateUserToken }) } { route: Register, method: Post, body } = defaultErrorHandlers do
  str <- fromAff $ toString body
  parsed <- expandErr $ parseUserFromJson str
  expandErr $ register parsed.user >>= updateUserToken >>= userToResponse

  where
  parseUserFromJson :: String -> Om {} (parsingError :: MultipleErrors) { user :: UserCreateInput }
  parseUserFromJson = throwLeftAsM (\err -> throw { parsingError: err }) <<< readJSON

mkUsersRouter _ { route: Register } = notFound

-- | login a user
-- NOTE: does not check if currently logged in.
-- NOTE: updates token
mkUsersRouter { userService: (UserService { login }), tokenService: (TokenService { updateUserToken }) } { route: Authen, method: Post, body } = defaultErrorHandlers do
  input <- fromAff $ toString body
  parsed <- expandErr $ parseinputFromJson input
  expandErr $ login parsed.user >>= updateUserToken >>= userToResponse

  where
  parseinputFromJson :: String -> Om {} (parsingError :: MultipleErrors) { user :: UserLoginInput }
  parseinputFromJson = throwLeftAsM (\err -> throw { parsingError: err }) <<< readJSON

mkUsersRouter _ { route: Authen } = notFound

-- | get logged in user
mkUsersRouter _ { route: Authed, method: Get, user } = defaultErrorHandlers do
  case user of
    Nothing -> forbidden
    Just user' -> userToResponse user'

-- | update the current user
mkUsersRouter { userService: (UserService { update }) } { route: Authed, method: Put, user, body } = defaultErrorHandlers do
  case user of
    Nothing -> forbidden
    Just userOutput' -> do
      input <- fromAff $ toString body
      parsed <- expandErr $ parseInputFromJson input
      expandErr $ update (Right userOutput'.username) parsed.user >>= userToResponse

  where
  parseInputFromJson :: String -> Om {} (parsingError :: MultipleErrors) { user :: UpdateUserInput }
  parseInputFromJson = throwLeftAsM (\err -> throw { parsingError: err }) <<< readJSON

mkUsersRouter _ { route: Authed } = notFound
