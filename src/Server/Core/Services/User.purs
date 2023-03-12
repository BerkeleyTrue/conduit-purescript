module Server.Core.Services.User where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Server.Core.Domain.User (User, UserId)
import Server.Core.Ports.Ports (UserRepo(..), UserCreateInput)

newtype UserService m = UserService
  { createUser :: UserCreateInput -> m (Either String User)
  , findUser :: UserId -> m (Either String User)
  }

findUser :: UserRepo Aff -> UserId -> Aff (Either String User)
findUser (UserRepo { getById }) userId = do
  res <- getById userId
  pure $ case res of
    Just user -> Right user
    Nothing -> Left "User not found"

createUser :: UserRepo Aff -> UserCreateInput -> Aff (Either String User)
createUser (UserRepo { create }) userReg = create userReg

mkUserService :: UserRepo Aff -> UserService Aff
mkUserService repo = UserService
  { createUser: createUser repo
  , findUser: findUser repo
  }
