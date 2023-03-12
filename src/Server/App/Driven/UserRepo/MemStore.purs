module Server.App.Driven.UserRepo.MemStore
  ( mkMemoryUserRepo
  ) where

import Prelude

import Control.Monad.List.Trans (foldl)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.UUID as UUID
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Effect.Aff.AVar as Ref
import Effect.Class (liftEffect)
import Effect.Now (nowDate)
import Server.Core.Domain.User (User, UserId(..), Username)
import Server.Core.Ports.Ports (UserRepo(..), UserCreateInput)

type UserMap = Map UserId User
type UsernameToIdMap = Map Username UserId

type MemStore =
  { byId :: UserMap
  , usernameToId :: UsernameToIdMap
  }

createUser :: AVar MemStore -> UserCreateInput -> Aff (Either String User)
createUser storeRef { username, email, password } = do
  store@{ byId } <- Ref.read storeRef
  userId <- liftEffect UUID.genUUID <#> (\uuid -> UserId uuid)
  createdNow <- liftEffect nowDate

  let
    user =
      { userId
      , username
      , email
      , password
      , createdAt: createdNow
      , updatedAt: Nothing
      , bio: Nothing
      , image: Nothing
      }
    newStore = store { byId = Map.insert userId user $ byId }

  Ref.put newStore storeRef
  pure $ Right user

getUserById :: AVar MemStore -> UserId -> Aff (Maybe User)
getUserById storeRef userId = do
  { byId } <- Ref.read storeRef
  pure $ Map.lookup userId byId

getUserByUsername :: AVar MemStore -> Username -> Aff (Maybe User)
getUserByUsername storeRef username = do
  { byId, usernameToId } <- Ref.read storeRef
  pure $ Map.lookup username usernameToId >>= flip Map.lookup byId

updateUserById :: AVar MemStore -> UserId -> (User -> User) -> Aff (Either String User)
updateUserById storeRef userId updateFn = do
  store@{ byId } <- Ref.read storeRef
  case Map.lookup userId byId of
    Nothing -> pure $ Left "User not found"
    Just user -> do
      let
        updatedUser = updateFn user
        newStore = store { byId = Map.insert userId updatedUser byId }
      Ref.put newStore storeRef
      pure $ Right updatedUser

mkMemoryUserRepo :: UserMap -> Aff (UserRepo Aff)
mkMemoryUserRepo initialState = do
  usernameToId <- foldl (\acc (userId /\ { username }) -> Map.insert username userId acc) Map.empty $ Map.toUnfoldable initialState
  storeRef <- Ref.new { byId: initialState, usernameToId }
  pure $ UserRepo
    { create: createUser storeRef
    , getById: getUserById storeRef
    , getByUsername: getUserByUsername storeRef
    , update: updateUserById storeRef
    }
