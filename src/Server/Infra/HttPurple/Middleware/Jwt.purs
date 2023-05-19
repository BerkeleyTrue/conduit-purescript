module Server.Infra.HttPurple.Middleware.Jwt
  ( mkAuthenticateJwtMiddleware
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (message)
import Effect.Class (liftEffect)
import Effect.Console (error, log)
import HTTPurple (RequestR, forbidden', internalServerError', jsonHeaders, lookup)
import HTTPurple.Middleware (Middleware)
import Prim.Row (class Nub, class Union)
import Record (merge)
import Server.Core.Services.Token (TokenService(..), JwtPayload)
import Server.Core.Services.User (UserOutput)
import Yoga.JSON (writeJSON)
import Yoga.Om (expandErr, fromAff, runOm)

-- | Authenticate a user using JWT
mkAuthenticateJwtMiddleware
  :: forall route extIn extOut
   . Union extIn (authed :: Maybe JwtPayload, user :: Maybe UserOutput) extOut
  => Nub (RequestR route extOut) (RequestR route extOut)
  => TokenService
  -> Middleware route extIn extOut
mkAuthenticateJwtMiddleware (TokenService { decode }) router request@{ headers } = runOm
  {}
  { exception: \err -> do
      liftEffect $ error $ message err
      internalServerError' jsonHeaders $ writeJSON { message: "Opps, something went wrong" }
  , jwtError: \err -> do
      liftEffect $ error $ show err
      forbidden' jsonHeaders
  , userRepoErr: \err -> do
      liftEffect $ error $ show err
      forbidden' jsonHeaders
  }
  case lookup headers "Authorization" of
    Nothing -> do
      liftEffect $ log "No Authorization header"
      fromAff $ router $ merge request { authed: Nothing :: Maybe JwtPayload, user: Nothing :: Maybe UserOutput }
    Just authHeader -> do
      (authed /\ user) <- expandErr $ decode authHeader
      fromAff $ router $ merge request { user: Just user, authed: Just authed }
