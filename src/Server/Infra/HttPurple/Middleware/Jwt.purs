module Server.Infra.HttPurple.Middleware.Jwt
  ( mkAuthenticateJwtMiddleware
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (message)
import Effect.Class (liftEffect)
import Effect.Console (error, log)
import HTTPurple (RequestR, forbidden', internalServerError', jsonHeaders, lookup)
import HTTPurple.Middleware (Middleware)
import Prim.Row (class Nub, class Union)
import Record (merge)
import Server.App.Api (JWTPayload)
import Server.Core.Services.User (UserOutput, UserService(..))
import Server.Infra.Yoga.JWT (Jwt(..), Secret, decodeJwt)
import Yoga.JSON (writeJSON)
import Yoga.Om (expandErr, fromAff, runOm)

-- | Authenticate a user using JWT
mkAuthenticateJwtMiddleware
  :: forall route extIn extOut
   . Union extIn (authed :: Maybe JWTPayload, user :: Maybe UserOutput) extOut
  => Nub (RequestR route extOut) (RequestR route extOut)
  => UserService
  -> Secret
  -> Middleware route extIn extOut
mkAuthenticateJwtMiddleware (UserService { getUser }) secret router request@{ headers } = runOm
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
      fromAff $ router $ merge request { authed: Nothing :: Maybe JWTPayload, user: Nothing :: Maybe UserOutput }
    Just jwt -> do
      (authed :: JWTPayload) <- expandErr $ decodeJwt secret (Jwt jwt)
      (user :: UserOutput) <- expandErr $ getUser authed.userId
      fromAff $ router $ merge request { user: Just user, authed: Just authed }
