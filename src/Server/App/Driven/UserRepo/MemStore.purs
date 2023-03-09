module Server.App.Driven.UserRepo.MemStore where

import Prelude

import Control.Monad.Cont (lift)
import Control.Monad.List.Trans (foldl)
import Data.Array (cons)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.UUID as UUID
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Effect.Aff.AVar as Ref
import Effect.Class (class MonadEffect, liftEffect)
import Server.Core.Domain.User (User(..), UserId(..), UserInfo(..), UserRegistration(..), Username)
import Server.Core.Ports.Ports (UserRepo(..))

type UserMap = Map UserId User
type UsernameToIdMap = Map Username UserId

type MemStore =
  { byId :: UserMap
  , usernameToId :: UsernameToIdMap
  }

createUser :: AVar MemStore -> UserRegistration -> Aff (Either String User)
createUser storeRef (UserRegistration { username, email, password }) = do
  store@{ byId } <- Ref.read storeRef
  uuid <- liftEffect UUID.genUUID
  let
    userInfo = UserInfo { username, email, bio: Nothing, image: Nothing }
    userRegistration = UserRegistration { username, email, password }
    user = User (UserId uuid) userInfo Nothing (Just userRegistration) Nothing
    newStore = store { byId = Map.insert (UserId uuid) user $ store.byId }

  Ref.put newStore storeRef
  pure $ Right user

getUserById :: AVar MemStore -> UserId -> Aff (Maybe User)
-- getUserById = Ref.read >=> pure <<< _.byId >>> Map.lookup
getUserById storeRef userId = do
  store@{ byId } <- Ref.read storeRef
  pure $ Map.lookup userId byId

getUserByUsername :: AVar MemStore -> Username -> Aff (Maybe User)
getUserByUsername storeRef username = do
  store@{ byId, usernameToId } <- Ref.read storeRef
  pure $ Map.lookup username usernameToId
    >>= flip Map.lookup byId

-- updateUserById :: AVar MemStore -> UserId -> User -> Aff (Maybe User)

mkMemoryUserRepo :: UserMap -> Aff (UserRepo Aff)
mkMemoryUserRepo initialState = do
  usernameToId <- foldl (\acc (_ /\ (User userId (UserInfo { username }) _ _ _)) -> Map.insert username userId acc) Map.empty $ Map.toUnfoldable initialState
  storeRef <- Ref.new { byId: initialState, usernameToId }
  pure $ UserRepo
    { create: createUser storeRef
    , getById: getUserById storeRef
    , getByUsername: getUserByUsername storeRef
    , update: ?_
    }
