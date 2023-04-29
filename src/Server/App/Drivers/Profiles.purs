module Server.App.Drivers.Profiles
  ( ProfilesRoute(..)
  , profilesRoute
  , mkProfilesRouter
  ) where

import Prelude hiding ((/))

import Conduit.Data.Username (Authorname)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import HTTPurple (Method(..), RouteDuplex', badRequest, notFound, ok, prefix, segment, sum, (/))
import Server.Core.Services.User (UserService(..))
import Server.Infra.Data.Route (usernameR)
import Server.Infra.HttPurple.Types (OmRouter)
import Yoga.Om (handleErrors)

data ProfilesRoute
  = Profile Authorname
  | Follow Authorname

derive instance genericProfilesRoute :: Generic ProfilesRoute _

profilesRoute :: RouteDuplex' ProfilesRoute
profilesRoute = prefix "profiles" $ sum
  { "Profile": usernameR segment
  , "Follow": usernameR segment / "follow"
  }

type ProfilesRouterDeps =
  { userService :: UserService }

-- TODO: get userId if authenticated
mkProfilesRouter :: forall ext. ProfilesRouterDeps -> OmRouter ProfilesRoute ext
mkProfilesRouter { userService: (UserService { getProfile }) } { method: Get, route: Profile authorname } = handleErrors errorHandlers do
  user <- getProfile (Right authorname) Nothing
  ok $ "Get " <> (show authorname) <> "'s profile" <> show user
  where
  errorHandlers = { userRepoErr: \err -> badRequest $ "Error getting profile: " <> show err }

mkProfilesRouter _ { route: Profile _ } = notFound

-- | Follow a user
mkProfilesRouter _ { method: Post, route: Follow authorname } = ok $ "Follow " <> (show authorname) <> "'s profile"

-- | Unfollow a user
mkProfilesRouter _ { method: Delete, route: Follow authorname } = ok $ "Unfollow " <> (show authorname) <> "'s profile"
mkProfilesRouter _ { route: Follow _ } = notFound
