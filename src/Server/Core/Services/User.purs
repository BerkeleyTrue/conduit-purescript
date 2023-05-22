module Server.Core.Services.User
  ( UserService(..)
  , UserLoginInput
  , UpdateUserInput
  , UserOutput
  , UserServiceErrs
  , UserCreateInputExt
  , PublicProfile
  , mkUserService
  , formatUserOutput
  , formatUserToPublicProfile
  ) where

import Prelude

import Conduit.Data.Password (HashedPassword, Password, comparePasswords, hashPassword)
import Conduit.Data.UserId (UserId, AuthorId)
import Conduit.Data.Username (Username, Authorname)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.JSDate (now)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Effect.Class (liftEffect)
import Server.Core.Domain.User (Email, User, Author)
import Server.Core.Ports.Ports (UserRepo(..), UserCreateInput)
import Yoga.Om (Om, expandErr, fromAff, throw, throwLeftAsM)

type UserCreateInputExt = UserCreateInput (password :: Password)

type UserLoginInput =
  { email :: String
  , password :: Password
  }

type UpdateUserInput =
  { email :: Maybe Email
  , username :: Maybe Username
  , image :: Maybe String
  , bio :: Maybe String
  , password :: Maybe Password
  }

-- | sent to authorized user
type UserOutput =
  { email :: Email
  , username :: Username
  , token :: String
  , bio :: String
  , image :: String
  }

-- | sent to any third party user
type PublicProfile =
  { username :: Username
  , bio :: String
  , image :: String
  , following :: Boolean
  }

type UserServiceErrs r = (userRepoErr :: String | r)

newtype UserService = UserService
  { register :: UserCreateInputExt -> Om {} (UserServiceErrs ()) UserOutput
  , login :: UserLoginInput -> Om {} (UserServiceErrs ()) UserOutput
  , getUser :: UserId -> Om {} (UserServiceErrs ()) UserOutput
  , getIdFromUsername :: Username -> Om {} (UserServiceErrs ()) UserId
  , getProfile :: (Either AuthorId Authorname) -> Maybe Username -> Om {} (UserServiceErrs ()) PublicProfile
  , update :: (Either UserId Username) -> UpdateUserInput -> Om {} (UserServiceErrs ()) UserOutput
  , follow :: UserId -> (Either AuthorId Authorname) -> Om {} (UserServiceErrs ()) PublicProfile
  , unfollow :: UserId -> (Either AuthorId Authorname) -> Om {} (UserServiceErrs ()) PublicProfile
  }

derive instance newtypeUserService :: Newtype (UserService) _

formatUserOutput :: User -> UserOutput
formatUserOutput { email, username, bio, image } =
  { email
  , username
  , bio: fromMaybe "" bio
  , image: fromMaybe "" image
  , token: "token"
  }

getIdFromUsername :: UserRepo -> Username -> Om {} (userRepoErr :: String) UserId
getIdFromUsername (UserRepo { getByUsername }) username = _.userId <$> getByUsername username

formatUserToPublicProfile :: Maybe UserId -> Author -> PublicProfile
formatUserToPublicProfile (Just userToFollow) { username, bio, image, following } =
  { username
  , bio: fromMaybe "" bio
  , image: fromMaybe "" image
  , following: any (\authorId -> authorId == userToFollow) following
  }
formatUserToPublicProfile Nothing { username, bio, image } =
  { username
  , bio: fromMaybe "" bio
  , image: fromMaybe "" image
  , following: false
  }

registerUser :: UserRepo -> UserCreateInputExt -> Om {} (userRepoErr :: String) UserOutput
registerUser (UserRepo { create }) input = do
  hashedPassword <- fromAff $ hashPassword input.password
  create input { password = hashedPassword } <#> formatUserOutput

-- | Login User
loginUser :: UserRepo -> UserLoginInput -> Om {} (userRepoErr :: String) UserOutput
loginUser (UserRepo { getByEmail }) { email, password } = do
  user@{ password: storedPassword } <- getByEmail email
  (isPasswordValid :: Boolean) <- (fromAff $ comparePasswords password storedPassword) >>= throwLeftAsM (\err -> throw { userRepoErr: "Error while comparing passwords: " <> err })

  if isPasswordValid then pure $ formatUserOutput user
  else throw { userRepoErr: "Invalid email or password" }

getUser :: UserRepo -> UserId -> Om {} (userRepoErr :: String) UserOutput
getUser (UserRepo { getById }) userId = getById userId <#> formatUserOutput

getUserProfile :: UserRepo -> (Either AuthorId Authorname) -> Maybe Username -> Om {} (userRepoErr :: String) PublicProfile
getUserProfile userRepo authorIdOrName username = do
  author <- case authorIdOrName of
    Left authorId -> getById authorId
    Right authorname -> getByUsername authorname

  userId <- case username of
    Nothing -> pure Nothing
    Just username' -> Just <$> getIdFromUsername userRepo username'

  pure $ formatUserToPublicProfile userId author
  where
  getByUsername = (unwrap userRepo).getByUsername
  getById = (unwrap userRepo).getById

-- TODO: add validation for password/email
updateUser :: UserRepo -> Either UserId Username -> UpdateUserInput -> Om {} (userRepoErr :: String) UserOutput
updateUser (UserRepo { update, getByUsername }) eitherIdOrName input = do
  now <- liftEffect $ now
  userId <- case eitherIdOrName of
    Left userId -> pure userId
    Right username -> getByUsername username >>= pure <<< _.userId
  (pass :: Maybe HashedPassword) <- expandErr $ updatePassword input.password
  update userId
    ( \user -> user
        { email = fromMaybe user.email input.email
        , username = fromMaybe user.username input.username
        , password = fromMaybe user.password pass
        , image = input.image >>= \image -> if image == "" then user.image else Just image
        , bio = input.bio >>= \bio -> if bio == "" then user.bio else Just bio
        , updatedAt = Just now
        }
    ) <#> formatUserOutput
  where
  updatePassword :: (Maybe Password) -> Om {} () (Maybe HashedPassword)
  updatePassword Nothing = pure $ Nothing
  updatePassword (Just password) = fromAff $ hashPassword password <#> Just

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

mkUserService :: UserRepo -> UserService
mkUserService repo = UserService
  { register: registerUser repo
  , login: loginUser repo
  , getUser: getUser repo
  , getIdFromUsername: getIdFromUsername repo
  , getProfile: getUserProfile repo
  , update: updateUser repo
  , follow: followUser repo
  , unfollow: unfollowUser repo
  }
