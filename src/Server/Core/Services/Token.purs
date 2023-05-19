module Server.Core.Services.Token
  ( TokenService(..)
  , TokenServiceErrs
  , JwtPayload
  , mkTokenService
  )
  where

import Prelude

import Conduit.Data.UserId (UserId)
import Data.JSDate (JSDate, now)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Server.Core.Services.User (UserService(..), UserOutput)
import Server.Infra.Yoga.JWT (Algorithm(..), Jwt(..), JwtError, Secret, decodeJwt, encodeJwt)
import Yoga.Om (Om, expandErr)

type JwtPayload = { userId :: UserId, iat :: JSDate }
type TokenServiceErrs = ( jwtError :: JwtError, userRepoErr :: String)

newtype TokenService = TokenService
  { encode :: UserId -> Effect Jwt
  , decode :: String -> Om {} TokenServiceErrs (Tuple JwtPayload UserOutput)
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

mkEncode :: Secret -> Algorithm -> UserId -> Effect Jwt
mkEncode secret algorithm userId = do
  iat <- now
  encodeJwt secret algorithm { userId, iat }


mkTokenService :: UserService -> Secret -> TokenService
mkTokenService userService secret = TokenService
  { encode: mkEncode secret HS256
  , decode: mkDecode userService secret
  }
