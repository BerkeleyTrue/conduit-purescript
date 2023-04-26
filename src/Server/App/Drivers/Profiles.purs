module Server.App.Drivers.Profiles
  ( ProfilesRoute(..)
  , profilesRoute
  , mkProfilesRouter
  ) where

import Prelude hiding ((/))

import Conduit.Data.Username (Authorname)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import HTTPurple (Method(..), RouteDuplex', badRequest, notFound, ok, prefix, segment, string, sum, (/))
import Server.Core.Services.User (UserService(..))
import Server.Infra.Data.Route (usernameR)
import Server.Infra.HttPurple.Types (OmRouter)
import Yoga.Om (handleErrors)

data ProfilesRoute
  = Profile Authorname
  | Follow String

derive instance genericProfilesRoute :: Generic ProfilesRoute _

profilesRoute :: RouteDuplex' ProfilesRoute
profilesRoute = prefix "profiles" $ sum
  { "Profile": usernameR segment
  , "Follow": string segment / "follow"
  }

type ProfilesRouterDeps =
  { userService :: UserService }

mkProfilesRouter :: ProfilesRouterDeps -> OmRouter ProfilesRoute
mkProfilesRouter { userService: (UserService { getProfile }) } { method: Get, route: Profile username } = handleErrors errorHandlers do
  user <- getProfile username Nothing
  ok $ "Get " <> (show username) <> "'s profile" <> show user
  where
  errorHandlers = { userRepoErr: \err -> badRequest $ "Error getting profile: " <> show err }

mkProfilesRouter _ { route: Profile _ } = notFound
mkProfilesRouter _ { method: Post, route: Follow username } = ok $ "Follow " <> username <> "'s profile"
mkProfilesRouter _ { method: Delete, route: Follow username } = ok $ "Unfollow " <> username <> "'s profile"
mkProfilesRouter _ { route: Follow _ } = notFound
