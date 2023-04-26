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
import Conduit.Data.Username (Username, Authorname)
import Data.Foldable (any)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Effect.Class (liftEffect)
import Effect.Now (nowDate)
import Server.Core.Domain.User (AuthorId, Email, User, UserId, Author)
import Server.Core.Ports.Ports (UserRepo(..), UserCreateInput)
import Yoga.Om (Om, fromAff, throw, throwLeftAsM)

type UserLoginInput =
  { username :: Username
  , email :: String
  , password :: String
  }

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

newtype UserService = UserService
  { register :: UserCreateInput -> Om {} (userRepoErr :: String) UserOutput
  , login :: UserLoginInput -> Om {} (userRepoErr :: String) UserOutput
  , getUser :: UserId -> Om {} (userRepoErr :: String) UserOutput
  , getProfile :: Authorname -> Maybe UserId -> Om {} (userRepoErr :: String) PublicProfile
  , getUsernameFromId :: UserId -> Om {} (userRepoErr :: String) Username
  , update :: UserId -> UpdateUserInput -> Om {} (userRepoErr :: String) UserOutput
  , follow :: UserId -> AuthorId -> Om {} (userRepoErr :: String) PublicProfile
  , unfollow :: UserId -> AuthorId -> Om {} (userRepoErr :: String) PublicProfile
  }

derive instance newtypeUserService :: Newtype (UserService) _

formatUserOutput :: User -> UserOutput
formatUserOutput { email, username, bio, image } = { email, username, bio, image, token: "token" }

formatUserToPublicProfile :: Maybe UserId -> Author -> PublicProfile
formatUserToPublicProfile (Just userToFollow) { username, bio, image, following } =
  { username
  , bio
  , image
  , following: any (\authorId -> authorId == userToFollow) following
  }
formatUserToPublicProfile Nothing { username, bio, image } = { username, bio, image, following: false }

registerUser :: UserRepo -> UserCreateInput -> Om {} (userRepoErr :: String) UserOutput
registerUser (UserRepo { create }) userReg = create userReg <#> formatUserOutput

loginUser :: UserRepo -> UserLoginInput -> Om {} (userRepoErr :: String) UserOutput
loginUser (UserRepo { getByEmail }) { username, email, password } = do
  { password: storedPassword, bio, image } <- getByEmail email
  (isPasswordValid :: Boolean) <- (fromAff $ comparePasswords password storedPassword) >>= throwLeftAsM (\err -> throw { userRepoErr: "Error while comparing passwords: " <> err })

  if isPasswordValid then pure { email, bio, image, username, token: "token" }
  else throw { userRepoErr: "Invalid email or password" }

getUser :: UserRepo -> UserId -> Om {} (userRepoErr :: String) UserOutput
getUser (UserRepo { getById }) userId = do
  { email, username, bio, image } <- getById userId
  pure { email, username, bio, image, token: "token" }

getUserProfile :: UserRepo -> Authorname -> Maybe UserId -> Om {} (userRepoErr :: String) PublicProfile
getUserProfile (UserRepo { getByUsername }) authorname userId = do
  author <- getByUsername authorname
  pure $ formatUserToPublicProfile userId author

getUsernameFromId :: UserRepo -> UserId -> Om {} (userRepoErr :: String) Username
getUsernameFromId (UserRepo { getById }) userId = do
  { username } <- getById userId
  pure username

-- TODO: add validation for password/email
updateUser :: UserRepo -> UserId -> UpdateUserInput -> Om {} (userRepoErr :: String) UserOutput
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

followUser :: UserRepo -> UserId -> AuthorId -> Om {} (userRepoErr :: String) PublicProfile
followUser (UserRepo { follow }) userId authorId =
  follow userId authorId <#> toProfile
  where
  toProfile = formatUserToPublicProfile (Just authorId)

unfollowUser :: UserRepo -> UserId -> AuthorId -> Om {} (userRepoErr :: String) PublicProfile
unfollowUser (UserRepo { unfollow }) userId authorId =
  unfollow userId authorId >>= pure <<< toProfile
  where
  toProfile = formatUserToPublicProfile (Just authorId)

mkUserService :: UserRepo -> Om {} () UserService
mkUserService repo = pure $ UserService
  { register: registerUser repo
  , login: loginUser repo
  , getUser: getUser repo
  , getProfile: getUserProfile repo
  , getUsernameFromId: getUsernameFromId repo
  , update: updateUser repo
  , follow: followUser repo
  , unfollow: unfollowUser repo
  }
