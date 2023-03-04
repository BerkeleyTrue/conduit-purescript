module Server.App.Api.Profiles where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import HTTPurple (Method(..), Request, ResponseM, RouteDuplex', notFound, ok, prefix, segment, string, sum, (/))

data ProfilesRoute
  = Profile String
  | Follow String

derive instance genericProfilesRoute :: Generic ProfilesRoute _

profilesRoute :: RouteDuplex' ProfilesRoute
profilesRoute = prefix "profiles" $ sum
  { "Profile": string segment
  , "Follow": string segment / "follow"
  }

profilesRouter :: Request ProfilesRoute -> ResponseM
profilesRouter { method: Get, route: Profile username } = ok $ "Get "<> username <> "'s profile"
profilesRouter { route: Profile _ } = notFound
profilesRouter { method: Post, route: Follow username } = ok $ "Follow "<> username <> "'s profile"
profilesRouter { method: Delete, route: Follow username } = ok $ "Unfollow "<> username <> "'s profile"
profilesRouter { route: Follow _ } = notFound
