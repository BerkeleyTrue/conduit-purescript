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
import Effect.Class (liftEffect)
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

userToResponse :: forall errs. Om {} (userRepoErr :: String | errs) UserOutput -> Om {} (userRepoErr :: String | errs) Response
userToResponse userOm = userOm >>= ok' jsonHeaders <<< writeJSON

mkUsersRouter :: forall ext. UserRouterDeps -> OmRouter UserRoute (UserRouterExt ext)
-- | register a new user
-- Note: does not check if currently logged in.
mkUsersRouter { userService: (UserService { register, getIdFromUsername }), tokenService: (TokenService { encode }) } { route: Register, method: Post, body } = defaultErrorHandlers do
  str <- fromAff $ toString body
  parsed <- expandErr $ parseUserFromJson str
  userOutput <- expandErr $ register parsed.user
  userId <- expandErr $ getIdFromUsername userOutput.username
  token <- liftEffect $ encode userId
  ok' jsonHeaders $ writeJSON $ userOutput { token = token }

  where
  parseUserFromJson :: String -> Om {} (parsingError :: MultipleErrors) { user :: UserCreateInput }
  parseUserFromJson = throwLeftAsM (\err -> throw { parsingError: err }) <<< readJSON

mkUsersRouter _ { route: Register } = notFound

-- | login a user
-- NOTE: does not check if currently logged in.
mkUsersRouter { userService: (UserService { login, getIdFromUsername }), tokenService: (TokenService { encode }) } { route: Authen, method: Post, body } = defaultErrorHandlers do
  input <- fromAff $ toString body
  parsed <- expandErr $ parseinputFromJson input
  userOutput <- expandErr $ login parsed.user
  userId <- expandErr $ getIdFromUsername userOutput.username
  token <- liftEffect $ encode userId
  ok' jsonHeaders $ writeJSON $ userOutput { token = token }

  where
  parseinputFromJson :: String -> Om {} (parsingError :: MultipleErrors) { user :: UserLoginInput }
  parseinputFromJson = throwLeftAsM (\err -> throw { parsingError: err }) <<< readJSON

mkUsersRouter _ { route: Authen } = notFound

mkUsersRouter _ { route: Authed, method: Get, user } = defaultErrorHandlers do
  case user of
    Nothing -> forbidden
    Just user' -> userToResponse $ pure $ user'

-- | update the current user
mkUsersRouter { userService: (UserService { update, getIdFromUsername }), tokenService: (TokenService { encode }) } { route: Authed, method: Put, user, body } = defaultErrorHandlers do
  case user of
    Nothing -> forbidden
    Just userOutput' -> do
      input <- fromAff $ toString body
      parsed <- expandErr $ parseInputFromJson input
      userOutput <- expandErr $ update (Right userOutput'.username) parsed.user
      userId <- expandErr $ getIdFromUsername userOutput.username
      token <- liftEffect $ encode userId
      ok' jsonHeaders $ writeJSON $ userOutput { token = token }

  where
  parseInputFromJson :: String -> Om {} (parsingError :: MultipleErrors) { user :: UpdateUserInput }
  parseInputFromJson = throwLeftAsM (\err -> throw { parsingError: err }) <<< readJSON

mkUsersRouter _ { route: Authed } = notFound
