module Server.App.Driven.UserRepo.MemStore
  ( mkMemoryUserRepo
  ) where

import Prelude

import Conduit.Control.Monad.Except (maybeThrow)
import Conduit.Data.Username (Username)
import Control.Monad.Except (catchError, runExceptT, throwError)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List (List(..), nub, filter)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.UUID as UUID
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Effect.Aff.AVar as Ref
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (nowDate)
import Server.Core.Domain.User (Email, User, UserId(..), AuthorId)
import Server.Core.Ports.Ports (UserRepo(..), UserCreateInput)
import Yoga.Om (Om, fromAff)

type UserMap = Map UserId User
type UsernameToIdMap = Map Username UserId
type EmailToIdMap = Map String UserId

type MemStore =
  { byId :: UserMap
  , usernameToId :: UsernameToIdMap
  , emailToId :: EmailToIdMap
  }

createUser :: AVar MemStore -> UserCreateInput -> Aff (Either String User)
createUser storeRef { username, email, password } = do
  store@{ byId } <- Ref.take storeRef
  userId <- liftEffect UUID.genUUID <#> (\uuid -> UserId uuid)
  createdNow <- liftEffect nowDate

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

getUserByEmail :: AVar MemStore -> Email -> Aff (Either String User)
getUserByEmail storeRef email = runExceptT do
  { byId, emailToId } <- liftAff $ Ref.read storeRef
  maybeThrow ("No user found for " <> show email) $ Map.lookup email emailToId >>= flip Map.lookup byId

updateUserById :: AVar MemStore -> UserId -> (User -> User) -> Aff (Either String User)
updateUserById storeRef userId updateFn = runExceptT do
  store@{ byId } <- liftAff $ Ref.take storeRef
  user <- maybeThrow ("No user found with id " <> show userId) $ Map.lookup userId byId

  let
    updatedUser = updateFn user
    newStore = store { byId = Map.insert userId updatedUser byId }

  liftAff $ Ref.put newStore storeRef

  -- catch error and put back old store
  catchError (pure updatedUser) \err -> do
    liftAff $ Ref.put store storeRef
    log $ "Error updating user: " <> show err
    throwError err


followUser :: AVar MemStore -> UserId -> AuthorId -> Aff (Either String User)
followUser storeRef userId authorId =
  updateUserById storeRef userId updateFn
  where
  updateFn = \user@{ following } -> user { following = nub $ Cons authorId following }

unfollowUser :: AVar MemStore -> UserId -> AuthorId -> Aff (Either String User)
unfollowUser storeRef userId authorId =
  updateUserById storeRef userId updateFn
  where
  updateFn = \user@{ following } -> user { following = filter (_ /= authorId) following }

mkMemoryUserRepo :: forall ctx. UserMap -> Om  { | ctx } () (UserRepo Aff)
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
