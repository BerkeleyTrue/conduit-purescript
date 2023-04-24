module Server.App.Driven.UserRepo.MemStore
  ( mkMemoryUserRepo
  ) where

import Prelude

import Conduit.Data.Username (Username)
import Control.Monad.Except (catchError, throwError)
import Data.Foldable (foldl)
import Data.List (List(..), nub, filter)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.UUID as UUID
import Effect.AVar (AVar)
import Effect.Aff.AVar as Ref
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (nowDate)
import Server.Core.Domain.User (Email, User, UserId(..), AuthorId)
import Server.Core.Ports.Ports (UserRepo(..), UserCreateInput)
import Yoga.Om (Om, fromAff, note)

type UserMap = Map UserId User
type UsernameToIdMap = Map Username UserId
type EmailToIdMap = Map String UserId

type MemStore =
  { byId :: UserMap
  , usernameToId :: UsernameToIdMap
  , emailToId :: EmailToIdMap
  }

createUser :: forall ctx. AVar MemStore -> UserCreateInput -> Om { | ctx } (userRepoErr :: String) User
createUser storeRef { username, email, password } = do
  store@{ byId } <- fromAff $ Ref.take storeRef
  userId <- liftEffect $ UUID.genUUID <#> (\uuid -> UserId uuid)
  createdNow <- liftEffect $ nowDate

  let
    user =
      { userId
      , username
      , email
      , password
      , following: Nil
      , createdAt: createdNow
      , updatedAt: Nothing
      , bio: Nothing
      , image: Nothing
      }
    -- TODO: add usernameToId and emailToId
    newStore = store { byId = Map.insert userId user $ byId }

  fromAff $ Ref.put newStore storeRef
  pure $ user

getUserById :: forall ctx. AVar MemStore -> UserId -> Om { | ctx } (userRepoErr :: String) User
getUserById storeRef userId = do
  { byId } <- fromAff $ Ref.read storeRef
  note { userRepoErr: "Could not find user with id " <> show userId } $ Map.lookup userId byId

getUserByUsername :: forall ctx. AVar MemStore -> Username -> Om { | ctx } (userRepoErr :: String) User
getUserByUsername storeRef username = do
  { byId, usernameToId } <- fromAff $ Ref.read storeRef
  note { userRepoErr: "No user found for " <> show username } $ Map.lookup username usernameToId >>= flip Map.lookup byId

getUserByEmail :: forall ctx. AVar MemStore -> Email -> Om { | ctx } (userRepoErr :: String) User
getUserByEmail storeRef email = do
  { byId, emailToId } <- fromAff $ Ref.read storeRef
  note { userRepoErr: "No user found for " <> show email } $ Map.lookup email emailToId >>= flip Map.lookup byId

updateUserById :: forall ctx. AVar MemStore -> UserId -> (User -> User) -> Om { | ctx } (userRepoErr :: String) User
updateUserById storeRef userId updateFn = do
  store@{ byId } <- fromAff $ Ref.take storeRef
  user <- note { userRepoErr: "No user found with id " <> show userId } $ Map.lookup userId byId

  let
    updatedUser = updateFn user
    newStore = store { byId = Map.insert userId updatedUser byId }

  fromAff $ Ref.put newStore storeRef

  -- catch error and put back old store
  catchError (pure updatedUser) \err -> do
    fromAff $ Ref.put store storeRef
    log $ "Error updating user: " <> show err
    throwError err

followUser :: forall ctx. AVar MemStore -> UserId -> AuthorId -> Om { | ctx } (userRepoErr :: String) User
followUser storeRef userId authorId =
  updateUserById storeRef userId updateFn
  where
  updateFn = \user@{ following } -> user { following = nub $ Cons authorId following }

unfollowUser :: forall ctx. AVar MemStore -> UserId -> AuthorId -> Om { | ctx } (userRepoErr :: String) User
unfollowUser storeRef userId authorId =
  updateUserById storeRef userId updateFn
  where
  updateFn = \user@{ following } -> user { following = filter (_ /= authorId) following }

mkMemoryUserRepo :: forall ctx. UserMap -> Om { | ctx } () (UserRepo ctx)
mkMemoryUserRepo initialState = do
  let
    { usernameToId, emailToId } =
      foldl
        ( \{ usernameToId, emailToId } { userId, username, email } ->
            { usernameToId: Map.insert username userId usernameToId
            , emailToId: Map.insert email userId emailToId
            }
        )
        { usernameToId: Map.empty
        , emailToId: Map.empty
        } $ Map.values initialState

  storeRef <- fromAff $ Ref.new { byId: initialState, usernameToId, emailToId }
  pure $ UserRepo
    { create: createUser storeRef
    , getById: getUserById storeRef
    , getByUsername: getUserByUsername storeRef
    , getByEmail: getUserByEmail storeRef
    , update: updateUserById storeRef
    , follow: followUser storeRef
    , unfollow: unfollowUser storeRef
    }
