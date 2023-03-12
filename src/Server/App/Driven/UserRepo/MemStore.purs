module Server.App.Driven.UserRepo.MemStore
  ( mkMemoryUserRepo
  ) where

import Prelude

import Conduit.Control.Monad.Except (maybeThrow)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.UUID as UUID
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Effect.Aff.AVar as Ref
import Effect.Aff.Class (liftAff)
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

getUserById :: AVar MemStore -> UserId -> Aff (Either String User)
getUserById storeRef userId = runExceptT do
  { byId } <- liftAff $ Ref.read storeRef
  maybeThrow ("Could not find user with id " <> show userId) $ Map.lookup userId byId

getUserByUsername :: AVar MemStore -> Username -> Aff (Either String User)
getUserByUsername storeRef username = runExceptT do
  { byId, usernameToId } <- liftAff $ Ref.read storeRef
  maybeThrow ("No user found for " <> show username) $ Map.lookup username usernameToId >>= flip Map.lookup byId

updateUserById :: AVar MemStore -> UserId -> (User -> User) -> Aff (Either String User)
updateUserById storeRef userId updateFn = runExceptT do
  store@{ byId } <- liftAff $ Ref.read storeRef
  user <- maybeThrow ("No user found with id " <> show userId) $ Map.lookup userId byId

  let
    updatedUser = updateFn user
    newStore = store { byId = Map.insert userId updatedUser byId }

  liftAff $ Ref.put newStore storeRef

  pure updatedUser

mkMemoryUserRepo :: UserMap -> Aff (UserRepo Aff)
mkMemoryUserRepo initialState = do
  let usernameToId = foldl (\acc { userId, username } -> Map.insert username userId acc) Map.empty $ Map.values initialState
  storeRef <- Ref.new { byId: initialState, usernameToId }
  pure $ UserRepo
    { create: createUser storeRef
    , getById: getUserById storeRef
    , getByUsername: getUserByUsername storeRef
    , update: updateUserById storeRef
    }
