module Server.Core.Services.User
  ( UserService(..)
  , UserLoginInput
  , UpdateUserInput
  , UserOutput
  , PublicProfile
  , mkUserService
  , formatUserOutput
  , formatUserToPublicProfile
  ) where

import Prelude

import Conduit.Data.Password (comparePasswords)
import Conduit.Data.Username (Username, Authorname)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.JSDate (now)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Effect.Class (liftEffect)
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
  , getProfile :: (Either AuthorId Authorname) -> Maybe UserId -> Om {} (userRepoErr :: String) PublicProfile
  , update :: (Either UserId Username) -> UpdateUserInput -> Om {} (userRepoErr :: String) UserOutput
  , follow :: UserId -> (Either AuthorId Authorname) -> Om {} (userRepoErr :: String) PublicProfile
  , unfollow :: UserId -> (Either AuthorId Authorname) -> Om {} (userRepoErr :: String) PublicProfile
  }

derive instance newtypeUserService :: Newtype (UserService) _

formatUserOutput :: User -> UserOutput
formatUserOutput { email, username, bio, image } = { email, username, bio, image, token: "token" }

getIdFromUsername :: UserRepo -> Username -> Om {} (userRepoErr :: String) UserId
getIdFromUsername (UserRepo { getByUsername }) username = getByUsername username >>= pure <<< _.userId

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

-- | Login User
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

getUserProfile :: UserRepo -> (Either AuthorId Authorname) -> Maybe UserId -> Om {} (userRepoErr :: String) PublicProfile
getUserProfile (UserRepo { getByUsername, getById }) authorIdOrName userId = do
  author <- case authorIdOrName of
    Left authorId -> getById authorId
    Right authorname -> getByUsername authorname

  pure $ formatUserToPublicProfile userId author

-- TODO: add validation for password/email
updateUser :: UserRepo -> Either UserId Username -> UpdateUserInput -> Om {} (userRepoErr :: String) UserOutput
updateUser (UserRepo { update, getByUsername }) eitherIdOrName input = do
  now <- liftEffect $ now
  userId <- case eitherIdOrName of
    Left userId -> pure userId
    Right username -> getByUsername username >>= pure <<< _.userId
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

followUser :: UserRepo -> UserId -> (Either AuthorId Authorname) -> Om {} (userRepoErr :: String) PublicProfile
followUser userRepo userId authorIdOrName = do
  authorId <- case authorIdOrName of
    Left authorId -> pure authorId
    Right authorname -> getIdFromUsername userRepo authorname

  follow userId authorId <#> formatUserToPublicProfile (Just authorId)
  where
  follow = (unwrap userRepo).follow

unfollowUser :: UserRepo -> UserId -> (Either AuthorId Authorname) -> Om {} (userRepoErr :: String) PublicProfile
unfollowUser userRepo userId authorIdOrName = do
  authorId <- case authorIdOrName of
    Left authorId -> pure authorId
    Right authorname -> getIdFromUsername userRepo authorname

  unfollow userId authorId <#> formatUserToPublicProfile (Just authorId)
  where
  unfollow = (unwrap userRepo).unfollow

mkUserService :: UserRepo -> Om {} () UserService
mkUserService repo = pure $ UserService
  { register: registerUser repo
  , login: loginUser repo
  , getUser: getUser repo
  , getProfile: getUserProfile repo
  , update: updateUser repo
  , follow: followUser repo
  , unfollow: unfollowUser repo
  }
