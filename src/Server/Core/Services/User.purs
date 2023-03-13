module Server.Core.Services.User where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Bifunctor (rmap)
import Data.Either (Either)
import Data.Foldable (any)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now (nowDate)
import Server.Core.Domain.User (AuthorId, Email, User, UserId, Username)
import Server.Core.Ports.Ports (UserRepo(..), UserCreateInput)
import Server.Infra.Data.Password (comparePasswords)

type UserLoginInput = { username :: Username, email :: String, password :: String }

type UpdateUserInput =
  { email :: Maybe Email
  , username :: Maybe Username
  , image :: Maybe String
  , bio :: Maybe String
  , password :: Maybe String
  }

-- | sent to authorized user
type UserOutput =
  { email :: Email
  , username :: Username
  , token :: String
  , bio :: Maybe String
  , image :: Maybe String
  }

-- | sent to any third party user
type PublicProfile =
  { username :: Username
  , bio :: Maybe String
  , image :: Maybe String
  , following :: Boolean
  }

newtype UserService m = UserService
  { register :: UserCreateInput -> m (Either String User)
  , login :: UserLoginInput -> m (Either String UserOutput)
  , getUser :: UserId -> m (Either String UserOutput)
  , getProfile :: AuthorId -> UserId -> m (Either String PublicProfile)
  , update :: UserId -> UpdateUserInput -> m (Either String UserOutput)
  , follow :: UserId -> AuthorId -> m (Either String PublicProfile)
  , unfollow :: UserId -> AuthorId -> m (Either String PublicProfile)
  }

formatUserOutput :: User -> UserOutput
formatUserOutput { email, username, bio, image } = { email, username, bio, image, token: "token" }

formatUserToPublicProfile :: Maybe AuthorId -> User -> PublicProfile
formatUserToPublicProfile (Just authorToFollow) { username, bio, image, following } =
  { username
  , bio
  , image
  , following: any (\authorId -> authorId == authorToFollow) following
  }
formatUserToPublicProfile Nothing { username, bio, image } = { username, bio, image, following: false }

registerUser :: forall m. MonadEffect m => UserRepo m -> UserCreateInput -> m (Either String User)
registerUser (UserRepo { create }) userReg = create userReg

loginUser :: UserRepo Aff -> UserLoginInput -> Aff (Either String UserOutput)
loginUser (UserRepo { getByEmail }) { username, email, password } = runExceptT do
  { password: storedPassword, bio, image } <- ExceptT $ getByEmail email
  isPasswordValid <- ExceptT $ comparePasswords password storedPassword

  if isPasswordValid then pure { email, bio, image, username, token: "token" }
  else throwError "Invalid email or password"

getUser :: forall m. MonadEffect m => UserRepo m -> UserId -> m (Either String UserOutput)
getUser (UserRepo { getById }) userId = runExceptT do
  { email, username, bio, image } <- ExceptT $ getById userId
  pure { email, username, bio, image, token: "token" }

-- TODO: add following field
getUserProfile :: forall m. MonadEffect m => UserRepo m -> AuthorId -> UserId -> m (Either String PublicProfile)
getUserProfile (UserRepo { getById }) _ userId = runExceptT do
  { username, bio, image } <- ExceptT $ getById userId
  pure $ { username, bio, image, following: false }

-- TODO: add validation for password/email
updateUser :: forall m. MonadEffect m => UserRepo m -> UserId -> UpdateUserInput -> m (Either String UserOutput)
updateUser (UserRepo { update }) userId input = do
  now <- liftEffect $ nowDate
  update userId
    ( \user -> user
        { email = fromMaybe user.email input.email
        , username = fromMaybe user.username input.username
        , password = fromMaybe user.password input.password
        , image = input.image >>= \image -> if image == "" then user.image else Just image
        , bio = input.bio >>= \bio -> if bio == "" then user.bio else Just bio
        , updatedAt = Just now
        }
    ) >>= pure <<< rmap formatUserOutput

followUser :: forall m. MonadEffect m => UserRepo m -> UserId -> AuthorId -> m (Either String PublicProfile)
followUser (UserRepo { follow }) userId authorId =
  follow userId authorId >>= pure <<< rmap toProfile
  where
    toProfile = formatUserToPublicProfile (Just authorId)

unfollowUser :: forall m. MonadEffect m => UserRepo m -> UserId -> AuthorId -> m (Either String PublicProfile)
unfollowUser (UserRepo { unfollow }) userId authorId =
  unfollow userId authorId >>= pure <<< rmap toProfile
  where
    toProfile = formatUserToPublicProfile (Just authorId)

mkUserService :: UserRepo Aff -> UserService Aff
mkUserService repo = UserService
  { register: registerUser repo
  , login: loginUser repo
  , getUser: getUser repo
  , getProfile: getUserProfile repo
  , update: updateUser repo
  , follow: followUser repo
  , unfollow: unfollowUser repo
  }
