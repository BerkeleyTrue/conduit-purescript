module Server.App.Drivers.Profiles
  ( ProfilesRoute(..)
  , profilesRoute
  , mkProfilesRouter
  ) where

import Prelude hiding ((/))

import Conduit.Data.Username (Authorname)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import HTTPurple (Method(..), Response, RouteDuplex', badRequest', jsonHeaders, notFound, ok, ok', prefix, segment, sum, (/))
import Server.Core.Services.User (UserOutput, UserService(..), UserServiceErrs)
import Server.Infra.Data.Route (usernameR)
import Server.Infra.HttPurple.Types (OmRouter)
import Yoga.JSON (writeJSON)
import Yoga.Om (Om, handleErrors)

data ProfilesRoute
  = Profile Authorname
  | Follow Authorname

derive instance genericProfilesRoute :: Generic ProfilesRoute _

profilesRoute :: RouteDuplex' ProfilesRoute
profilesRoute = prefix "profiles" $ sum
  { "Profile": usernameR segment
  , "Follow": usernameR segment / "follow"
  }

type ProfilesRouterExts ext = (user :: Maybe UserOutput | ext)
type ProfilesRouterDeps =
  { userService :: UserService }

defaultErrorHandlers
  :: forall ctx errOut
   . Om ctx (UserServiceErrs errOut) Response
  -> Om ctx errOut Response
defaultErrorHandlers = handleErrors
  { userRepoErr: \err -> badRequest' jsonHeaders $ writeJSON { message: show err }
  }

mkProfilesRouter :: forall ext. ProfilesRouterDeps -> OmRouter ProfilesRoute (ProfilesRouterExts ext)
mkProfilesRouter { userService: (UserService { getProfile }) } { method: Get, route: Profile authorname, user } = defaultErrorHandlers do
  author <- getProfile (Right authorname) $ user <#> _.username
  ok' jsonHeaders $ writeJSON author

mkProfilesRouter _ { route: Profile _ } = notFound

-- | Follow a user
mkProfilesRouter _ { method: Post, route: Follow authorname } = ok $ "Follow " <> (show authorname) <> "'s profile"

-- | Unfollow a user
mkProfilesRouter _ { method: Delete, route: Follow authorname } = ok $ "Unfollow " <> (show authorname) <> "'s profile"
mkProfilesRouter _ { route: Follow _ } = notFound
