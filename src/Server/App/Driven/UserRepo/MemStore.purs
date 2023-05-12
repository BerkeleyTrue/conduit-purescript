module Server.App.Driven.UserRepo.MemStore
  ( mkMemoryUserRepo
  ) where

import Prelude

import Conduit.Data.UserId (AuthorId, UserId, mkUserId)
import Conduit.Data.Username (Username)
import Control.Monad.Except (catchError, throwError)
import Data.Array (filter, nub, snoc)
import Data.Foldable (foldl)
import Data.JSDate (now)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.AVar (AVar)
import Effect.Aff.AVar as Ref
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Server.Core.Domain.User (Email, User)
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

mkCreate :: AVar MemStore -> UserCreateInput -> Om {} (userRepoErr :: String) User
mkCreate storeRef { username, email, password } = do
  store@{ byId, usernameToId, emailToId } <- fromAff $ Ref.take storeRef
  userId <- liftEffect $ mkUserId
  createdNow <- liftEffect $ now

  let
    user =
      { userId
      , username
      , email
      , password
      , following: []
      , createdAt: createdNow
      , updatedAt: Nothing
      , bio: Nothing
      , image: Nothing
      }
    newStore = store
      { byId = Map.insert userId user $ byId
      , usernameToId = Map.insert username userId usernameToId
      , emailToId = Map.insert email userId emailToId
      }

  fromAff $ Ref.put newStore storeRef
  pure $ user

mkGetById :: AVar MemStore -> UserId -> Om {} (userRepoErr :: String) User
mkGetById storeRef userId = do
  { byId } <- fromAff $ Ref.read storeRef
  note { userRepoErr: "Could not find user with id " <> show userId } $ Map.lookup userId byId

mkGetByUsername :: AVar MemStore -> Username -> Om {} (userRepoErr :: String) User
mkGetByUsername storeRef username = do
  { byId, usernameToId } <- fromAff $ Ref.read storeRef
  note { userRepoErr: "No user found for " <> show username } $ Map.lookup username usernameToId >>= flip Map.lookup byId

mkGetByEmail :: AVar MemStore -> Email -> Om {} (userRepoErr :: String) User
mkGetByEmail storeRef email = do
  { byId, emailToId } <- fromAff $ Ref.read storeRef
  note { userRepoErr: "No user found for " <> show email } $ Map.lookup email emailToId >>= flip Map.lookup byId

mkUpdateById :: AVar MemStore -> UserId -> (User -> User) -> Om {} (userRepoErr :: String) User
mkUpdateById storeRef userId updateFn = do
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

mkFollow :: AVar MemStore -> UserId -> AuthorId -> Om {} (userRepoErr :: String) User
mkFollow storeRef userId authorId =
  mkUpdateById storeRef userId updateFn
  where
  updateFn = \user@{ following } -> user { following = nub $ snoc following authorId }

mkUnfollow :: AVar MemStore -> UserId -> AuthorId -> Om {} (userRepoErr :: String) User
mkUnfollow storeRef userId authorId =
  mkUpdateById storeRef userId updateFn
  where
  updateFn = \user@{ following } -> user { following = filter (_ /= authorId) following }

mkMemoryUserRepo :: UserMap -> Om {} () UserRepo
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
    { create: mkCreate storeRef
    , getById: mkGetById storeRef
    , getByUsername: mkGetByUsername storeRef
    , getByEmail: mkGetByEmail storeRef
    , update: mkUpdateById storeRef
    , follow: mkFollow storeRef
    , unfollow: mkUnfollow storeRef
    }
