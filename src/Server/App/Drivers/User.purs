module Server.App.Drivers.User
  ( UserRoute(..)
  , userRoute
  , mkUserRouter
  , UserRouterDeps
  ) where

import Prelude hiding ((/))

import Control.Monad.Cont (lift)
import Control.Monad.Except (except)
import Control.Monad.Except.Checked (ExceptV, handleError, safe)
import Data.Bifunctor (bimap, lmap, rmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (NonEmptyList)
import Data.Variant (inj, Variant)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign (ForeignError, MultipleErrors)
import HTTPurple (Method(..), Response, RouteDuplex', ResponseM, badRequest, noArgs, notFound, ok, sum, toString, (/))
import HTTPurple.Body (RequestBody)
import Server.Core.Domain.User (User)
import Server.Core.Ports.Ports (UserCreateInput)
import Server.Core.Services.User (UserService(..))
import Server.Infra.HttPurple.Types (Router)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
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

type RegisterError r = (registerError :: String | r)
type ParseError r = (parseError :: NonEmptyList ForeignError | r)

registerError :: forall r. String -> Variant (RegisterError + r)
registerError = inj (Proxy :: Proxy "registerError")

parseError :: forall r. NonEmptyList ForeignError -> Variant (ParseError + r)
parseError errors = inj (Proxy :: Proxy "parseError") errors


mkUserRouter :: UserRouterDeps Aff -> Router UserRoute
mkUserRouter { userService: (UserService { register }) } { route: Register, method: Post, body } = do
  safe $ handleRegisterError $ registerUser body >>= userToExceptT
  where
  readUserJSON :: String -> Either MultipleErrors { user :: UserCreateInput }
  readUserJSON = readJSON

  registerUser :: forall r. RequestBody -> ExceptV (ParseError + RegisterError + r) Aff User
  registerUser body' = do
    parsed <- except =<< lmap parseError <<< readUserJSON <$> toString body'
    except =<< lift (lmap registerError <$> register parsed.user)

  handleRegisterUserError :: forall r. String -> ExceptV r Aff Response
  handleRegisterUserError = \err -> do
    liftEffect $ log $ "Register Error: " <> show err
    badRequest $ show err

  handleParseError :: forall r. NonEmptyList ForeignError -> ExceptV r Aff Response
  handleParseError = \err -> do
    liftEffect $ log $ "Parse Error: " <> show err
    badRequest $ show err

  handleSuccess :: User -> ResponseM
  handleSuccess = \user -> do
    liftEffect $ log $ "Register Success: " <> show user
    ok $ show user

  userToExceptT :: forall e. User -> ExceptV e Aff Response
  userToExceptT user = do
    res <- lift $ handleSuccess user
    pure $ res

  handleRegisterError = handleError
    { registerUserError: handleRegisterUserError
    , parseError: handleParseError
    }


mkUserRouter _ { route: Register } = notFound

mkUserRouter _ { route: Authen, method: Post } = ok "authenticate"
mkUserRouter _ { route: Authen } = notFound

mkUserRouter _ { route: Authed, method: Put } = ok "Update user"
mkUserRouter _ { route: Authed, method: Get } = ok "Find user"
mkUserRouter _ { route: Authed } = notFound
