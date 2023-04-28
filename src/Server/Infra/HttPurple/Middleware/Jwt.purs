module Server.Infra.HttPurple.Middleware.Jwt where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (message)
import Effect.Class (liftEffect)
import Effect.Console (error, log)
import HTTPurple (RequestR, forbidden', internalServerError', jsonHeaders, lookup)
import HTTPurple.Middleware (Middleware)
import Prim.Row (class Nub, class Union)
import Record (merge)
import Server.Core.Domain.User (User)
import Server.Infra.HttPurple.Types (Middleware', Router)
import Server.Infra.Yoga.JWT (Jwt(..), Secret, decode)
import Yoga.JSON (writeJSON)
import Yoga.Om (fromAff, runOm)

-- | Authenticate a user using JWT
-- mkAuthenticateUserMiddleware
--   :: forall route extIn extOut
--    . Nub (RequestR route extOut) (RequestR route extOut)
--   => Union extIn (user :: Maybe User) extOut
--   => Secret
--   -> Middleware route extIn extOut
mkAuthenticateUserMiddleware secret router request@{ headers } = runOm
  {}
  { exception: \err -> do
      liftEffect $ error $ message err
      internalServerError' jsonHeaders $ writeJSON { message: "Opps, something went wrong" }
  , jwtError: \err -> do
      liftEffect $ error $ show err
      forbidden' jsonHeaders
  }
  do
    case lookup headers "Authorization" of
      Nothing -> do
        liftEffect $ log "No Authorization header"
        fromAff $ router $ merge request { user: Nothing }
      Just jwt -> do
        (user :: User) <- decode secret (Jwt jwt)
        fromAff $ router $ merge request { user: (Just user) }
