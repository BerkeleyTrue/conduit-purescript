module Server.Core.Services.Token
  ( TokenService(..)
  , TokenServiceErrs
  , JwtPayload
  , mkTokenService
  ) where

import Prelude

import Conduit.Data.UserId (UserId)
import Data.JSDate (JSDate, now)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Server.Core.Services.User (UserService(..), UserOutput)
import Server.Infra.Yoga.JWT (Algorithm(..), Jwt(..), JwtError, Secret, decodeJwt, encodeJwt)
import Yoga.Om (Om, expandErr)

type JwtPayload = { userId :: UserId, iat :: JSDate }
type TokenServiceErrs = (jwtError :: JwtError, userRepoErr :: String)

newtype TokenService = TokenService
  { encode :: UserId -> Effect Jwt
  , decode :: String -> Om {} TokenServiceErrs (Tuple JwtPayload UserOutput)
  , updateUserToken :: UserOutput -> Om {} (userRepoErr :: String) UserOutput
  }

mkDecode :: UserService -> Secret -> String -> Om {} TokenServiceErrs (Tuple JwtPayload UserOutput)
mkDecode (UserService { getUser }) secret token = do
  ({ userId, iat } :: JwtPayload) <- expandErr $ decodeJwt secret (Jwt token)
  user <- expandErr $ getUser userId
  pure $
    ( Tuple
        { userId
        , iat
        }
        user
    )

mkEncode :: Secret -> UserId -> Effect Jwt
mkEncode secret userId = do
  iat <- now
  encodeJwt secret HS512 { userId, iat }

mkUserTokenUpdate :: UserService -> Secret -> UserOutput -> Om {} (userRepoErr :: String) UserOutput
mkUserTokenUpdate (UserService { getIdFromUsername }) secret user = do
  userId <- expandErr $ getIdFromUsername user.username
  (Jwt token) <- liftEffect $ mkEncode secret userId
  pure $ user { token = token }

mkTokenService :: UserService -> Secret -> TokenService
mkTokenService userService secret = TokenService
  { encode: mkEncode secret
  , decode: mkDecode userService secret
  , updateUserToken: mkUserTokenUpdate userService secret
  }
