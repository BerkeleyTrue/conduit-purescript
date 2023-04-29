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
import Server.Infra.Yoga.JWT (Jwt(..), Secret, decodeJwt)
import Yoga.JSON (class ReadForeign, writeJSON)
import Yoga.Om (fromAff, runOm)

-- | Authenticate a user using JWT
mkAuthenticateUserMiddleware
  :: forall user route extIn extOut
   . ReadForeign user
  => Nub (RequestR route extOut) (RequestR route extOut)
  => Union extIn (user :: Maybe user) extOut
  => Secret
  -> Middleware route extIn extOut
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
        fromAff $ router $ merge request { user: Nothing :: Maybe user }
      Just jwt -> do
        (user :: user) <- decodeJwt secret (Jwt jwt)
        fromAff $ router $ merge request { user: (Just user) :: Maybe user }
