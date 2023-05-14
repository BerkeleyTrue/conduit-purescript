module Server.App.Drivers.User
  ( UserRoute(..)
  , userRoute
  , mkUserRouter
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

userToResponse :: forall errs. Om {} (userRepoErr :: String | errs) UserOutput -> Om {} (userRepoErr :: String | errs) Response
userToResponse userOm = userOm >>= ok' jsonHeaders <<< writeJSON

mkUserRouter :: forall ext. UserRouterDeps -> OmRouter UserRoute (UserRouterExt ext)
-- | register a new user
-- Note: does not check if currently logged in.
mkUserRouter { userService: (UserService { register }) } { route: Register, method: Post, body } = defaultErrorHandlers do
  str <- fromAff $ toString body
  parsed <- expandErr $ parseUserFromJson str
  expandErr $ (userToResponse <<< register) parsed.user

  where
  parseUserFromJson :: String -> Om {} (parsingError :: MultipleErrors) { user :: UserCreateInput }
  parseUserFromJson = throwLeftAsM (\err -> throw { parsingError: err }) <<< readJSON

mkUserRouter _ { route: Register } = notFound

-- | login a user
-- NOTE: does not check if currently logged in.
mkUserRouter { userService: (UserService { login }) } { route: Authen, method: Post, body } = defaultErrorHandlers do
  input <- fromAff $ toString body
  parsed <- expandErr $ parseinputFromJson input
  expandErr $ (userToResponse <<< login) parsed.user

  where
  parseinputFromJson :: String -> Om {} (parsingError :: MultipleErrors) { user :: UserLoginInput }
  parseinputFromJson = throwLeftAsM (\err -> throw { parsingError: err }) <<< readJSON

mkUserRouter _ { route: Authen } = notFound

mkUserRouter _ { route: Authed, method: Get, user } = defaultErrorHandlers do
  case user of
    Nothing -> forbidden
    Just user' -> userToResponse $ pure $ user'

-- | update the current user
mkUserRouter { userService: (UserService { update }) } { route: Authed, method: Put, user, body } = defaultErrorHandlers do
  case user of
    Nothing -> forbidden
    Just userOutput -> do
      input <- fromAff $ toString body
      parsed <- expandErr $ parseInputFromJson input
      (userToResponse <<< expandErr <<< update (Right userOutput.username)) parsed.user
  where
  parseInputFromJson :: String -> Om {} (parsingError :: MultipleErrors) { user :: UpdateUserInput }
  parseInputFromJson = throwLeftAsM (\err -> throw { parsingError: err }) <<< readJSON

mkUserRouter _ { route: Authed } = notFound
