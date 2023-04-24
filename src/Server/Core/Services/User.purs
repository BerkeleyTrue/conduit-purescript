module Server.Core.Services.User
  ( UserService(..)
  , UserLoginInput
  , UpdateUserInput
  , UserOutput
  , PublicProfile
  , mkUserService
  ) where

import Prelude

import Conduit.Data.Password (comparePasswords)
import Conduit.Data.Username (Username)
import Data.Foldable (any)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Effect.Class (liftEffect)
import Effect.Now (nowDate)
import Server.Core.Domain.User (AuthorId, Email, User, UserId)
import Server.Core.Ports.Ports (UserRepo(..), UserCreateInput)
import Yoga.Om (Om, fromAff, throw, throwLeftAsM)

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

newtype UserService ctx = UserService
  { register :: UserCreateInput -> Om { | ctx } (userRepoErr :: String) UserOutput
  , login :: UserLoginInput -> Om { | ctx } (userRepoErr :: String) UserOutput
  , getUser :: UserId -> Om { | ctx } (userRepoErr :: String) UserOutput
  , getProfile :: AuthorId -> UserId -> Om { | ctx } (userRepoErr :: String) PublicProfile
  , update :: UserId -> UpdateUserInput -> Om { | ctx } (userRepoErr :: String) UserOutput
  , follow :: UserId -> AuthorId -> Om { | ctx } (userRepoErr :: String) PublicProfile
  , unfollow :: UserId -> AuthorId -> Om { | ctx } (userRepoErr :: String) PublicProfile
  }

derive instance newtypeUserService :: Newtype (UserService m) _

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

registerUser :: forall ctx. UserRepo ctx -> UserCreateInput -> Om { | ctx } (userRepoErr :: String) UserOutput
registerUser (UserRepo { create }) userReg = create userReg <#> formatUserOutput

loginUser :: forall ctx. UserRepo ctx -> UserLoginInput -> Om { | ctx } (userRepoErr :: String) UserOutput
loginUser (UserRepo { getByEmail }) { username, email, password } = do
  { password: storedPassword, bio, image } <- getByEmail email
  (isPasswordValid :: Boolean) <- (fromAff $ comparePasswords password storedPassword) >>= throwLeftAsM (\err -> throw { userRepoErr: "Error while comparing passwords: " <> err })

  if isPasswordValid then pure { email, bio, image, username, token: "token" }
  else throw { userRepoErr: "Invalid email or password" }

getUser :: forall ctx. UserRepo ctx -> UserId -> Om { | ctx } (userRepoErr :: String) UserOutput
getUser (UserRepo { getById }) userId = do
  { email, username, bio, image } <- getById userId
  pure { email, username, bio, image, token: "token" }

-- TODO: add following field
getUserProfile :: forall ctx. UserRepo ctx -> AuthorId -> UserId -> Om { | ctx } (userRepoErr :: String) PublicProfile
getUserProfile (UserRepo { getById }) _ userId = do
  { username, bio, image } <- getById userId
  pure $ { username, bio, image, following: false }

-- TODO: add validation for password/email
updateUser :: forall ctx. UserRepo ctx -> UserId -> UpdateUserInput -> Om { | ctx } (userRepoErr :: String) UserOutput
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
    ) >>= pure <<< formatUserOutput

followUser :: forall ctx. UserRepo ctx -> UserId -> AuthorId -> Om { | ctx } (userRepoErr :: String) PublicProfile
followUser (UserRepo { follow }) userId authorId =
  follow userId authorId <#> toProfile
  where
  toProfile = formatUserToPublicProfile (Just authorId)

unfollowUser :: forall ctx. UserRepo ctx -> UserId -> AuthorId -> Om { | ctx } (userRepoErr :: String) PublicProfile
unfollowUser (UserRepo { unfollow }) userId authorId =
  unfollow userId authorId >>= pure <<< toProfile
  where
  toProfile = formatUserToPublicProfile (Just authorId)

mkUserService :: forall ctx. UserRepo ctx -> Om { | ctx } () (UserService ctx)
mkUserService repo = pure $ UserService
  { register: registerUser repo
  , login: loginUser repo
  , getUser: getUser repo
  , getProfile: getUserProfile repo
  , update: updateUser repo
  , follow: followUser repo
  , unfollow: unfollowUser repo
  }
