module Server.Core.Services.User where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Server.Core.Domain.User (AuthorId, Email, PublicProfile, User, UserId, Username)
import Server.Core.Ports.Ports (UserRepo(..), UserCreateInput)
import Server.Infra.Data.Password (comparePasswords)

newtype UserService m = UserService
  { registerUser :: UserCreateInput -> m (Either String User)
  , getUserProfile :: AuthorId -> UserId -> m (Either String PublicProfile)
  }

registerUser :: forall m. Monad m => UserRepo m -> UserCreateInput -> m (Either String User)
registerUser (UserRepo { create }) userReg = create userReg

-- TODO: add following field
getUserProfile :: forall m. Monad m => UserRepo m -> AuthorId -> UserId -> m (Either String PublicProfile)
getUserProfile (UserRepo { getById }) _ userId = runExceptT do
  { username, bio, image } <- ExceptT $ getById userId
  pure $ { username, bio, image, following: false }

type UserLoginInput = { username :: Username, email :: String, password :: String }

type UserLoginOutput =
  { email :: Email
  , token :: String
  , username :: Username
  , bio :: Maybe String
  , image :: Maybe String
  }

loginUser :: UserRepo Aff -> UserLoginInput -> Aff (Either String UserLoginOutput)
loginUser (UserRepo { getByEmail }) { username, email, password } = runExceptT do
  { password: storedPassword, bio, image } <- ExceptT $ getByEmail email
  isPasswordValid <- ExceptT $ comparePasswords password storedPassword

  if isPasswordValid then pure { email, bio, image, username, token: "token" }
  else throwError "Invalid email or password"

mkUserService :: UserRepo Aff -> UserService Aff
mkUserService repo = UserService
  { registerUser: registerUser repo
  , getUserProfile: getUserProfile repo
  }
