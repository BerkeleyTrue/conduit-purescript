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

type UserLoginInput = { username :: Username, email :: String, password :: String }

type UserOutput =
  { email :: Email
  , username :: Username
  , token :: String
  , bio :: Maybe String
  , image :: Maybe String
  }

type UpdateUserInput =
  { email :: Maybe Email
  , username :: Maybe Username
  , image :: Maybe String
  , bio :: Maybe String
  , password :: Maybe String
  }

newtype UserService m = UserService
  { register :: UserCreateInput -> m (Either String User)
  , login :: UserLoginInput -> m (Either String UserOutput)
  , getUser :: UserId -> m (Either String UserOutput)
  , getProfile :: AuthorId -> UserId -> m (Either String PublicProfile)
  -- , update :: UserId -> UpdateUserInput -> m (Either String UserOutput)
  }

registerUser :: forall m. Monad m => UserRepo m -> UserCreateInput -> m (Either String User)
registerUser (UserRepo { create }) userReg = create userReg

loginUser :: UserRepo Aff -> UserLoginInput -> Aff (Either String UserOutput)
loginUser (UserRepo { getByEmail }) { username, email, password } = runExceptT do
  { password: storedPassword, bio, image } <- ExceptT $ getByEmail email
  isPasswordValid <- ExceptT $ comparePasswords password storedPassword

  if isPasswordValid then pure { email, bio, image, username, token: "token" }
  else throwError "Invalid email or password"

getUser :: forall m. Monad m => UserRepo m -> UserId -> m (Either String UserOutput)
getUser (UserRepo { getById }) userId = runExceptT do
  { email, username, bio, image } <- ExceptT $ getById userId
  pure { email, username, bio, image, token: "token" }

-- TODO: add following field
getUserProfile :: forall m. Monad m => UserRepo m -> AuthorId -> UserId -> m (Either String PublicProfile)
getUserProfile (UserRepo { getById }) _ userId = runExceptT do
  { username, bio, image } <- ExceptT $ getById userId
  pure $ { username, bio, image, following: false }

-- updateUser :: forall m. Monad m => UserRepo m -> UserId -> UpdateUserInput -> m (Either String UserOutput)
-- updateUser (UserRepo { update }) userId input = runExceptT do

mkUserService :: UserRepo Aff -> UserService Aff
mkUserService repo = UserService
  { register: registerUser repo
  , login: loginUser repo
  , getUser: getUser repo
  , getProfile: getUserProfile repo
  }
