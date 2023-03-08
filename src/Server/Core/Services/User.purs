module Server.Core.Services.User where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Server.Core.Domain.User (User, UserId)
import Server.Core.Ports.Ports (UserRepo(..))

newtype UserService m = UserService
  { createUser :: User -> m (Either String User)
  , findUser :: UserId -> m (Either String User)
  }

findUser :: forall m. MonadEffect m => UserRepo m -> UserId -> m (Either String User)
findUser (UserRepo { getById }) userId = do
  res <- getById userId
  pure $ case res of
    Just user -> Right user
    Nothing -> Left "User not found"

createUser :: forall m. MonadEffect m => UserRepo m -> User -> m (Either String User)
createUser (UserRepo { create }) user = do
  res <- create user
  pure $ case res of
    Right _ -> Right user
    Left _ -> Left "User already exists"

mkUserService :: forall m. MonadEffect m => UserRepo m -> UserService m
mkUserService repo = UserService
  { createUser: createUser repo
  , findUser: findUser repo
  }
