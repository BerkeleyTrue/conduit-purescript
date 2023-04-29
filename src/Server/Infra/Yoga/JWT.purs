-- source: https://github.com/oreshinya/purescript-simple-jwt/blob/v4.0.1/src/Node/Simple/Jwt.purs but using yoga-json
module Server.Infra.Yoga.JWT
  ( Secret
  , Jwt(..)
  , Algorithm(..)
  , JwtError(..)
  , decodeJwt
  , encodeJwt
  ) where

import Prelude

import Data.Array (replicate)
import Data.Either (Either)
import Data.String (Pattern(..), joinWith, length, split)
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign (MultipleErrors)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Crypto.Hmac as Hmac
import Node.Encoding (Encoding(..))
import Yoga.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)
import Yoga.Om (Om, throw, throwLeftAsM)

type Secret = String

-- | The type of JSON Web Token.
newtype Jwt = Jwt String

derive newtype instance showJwt :: Show Jwt
derive newtype instance eqJwt :: Eq Jwt
derive newtype instance readForeignJwt :: ReadForeign Jwt
derive newtype instance writeForeignJwt :: WriteForeign Jwt

-- | Supported algorithms.
data Algorithm
  = HS256
  | HS512

instance showAlgorithm :: Show Algorithm where
  show HS256 = "HS256"
  show HS512 = "HS512"

derive instance eqAlgorithm :: Eq Algorithm

-- | Errors for decoding.
data JwtError
  = InvalidTokenError
  | NotSupportedAlgorithmError
  | DecodeError
  | VerifyError

instance showJwtError :: Show JwtError where
  show InvalidTokenError = "InvalidTokenError"
  show NotSupportedAlgorithmError = "NotSupportedAlgorithmError"
  show DecodeError = "DecodeError"
  show VerifyError = "VerifyError"

derive instance eqJwtError :: Eq JwtError

-- | Decode JWT with signature verification.
decodeJwt :: forall payload. ReadForeign payload => Secret -> Jwt -> Om {} (jwtError :: JwtError) payload
decodeJwt secret (Jwt jwt) =
  case split (Pattern ".") jwt of
    [ headerSegment, payloadSegment, signatureSegment ] -> do
      alg <- readAlgorithm headerSegment
      isValid <- liftEffect $ verify secret alg (headerSegment <> "." <> payloadSegment) signatureSegment
      if isValid then readPayload payloadSegment
      else throw { jwtError: VerifyError }
    _ -> throw { jwtError: InvalidTokenError }

verify :: Secret -> Algorithm -> String -> String -> Effect Boolean
verify secret alg input signatureSegment = do
  signatureSegment' <- sign secret alg input
  pure $ signatureSegment == signatureSegment'

readPayload :: forall payload. ReadForeign payload => String -> Om {} (jwtError :: JwtError) payload
readPayload payloadSegment = do
  (liftEffect $ parsePayload payloadSegment) >>= throwLeftAsM (\_ -> throw { jwtError: DecodeError })
  where
  parsePayload :: String -> Effect (Either MultipleErrors payload)
  parsePayload = (pure <<< readJSON) <=< base64URLDecode

readAlgorithm :: String -> Om {} (jwtError :: JwtError) Algorithm
readAlgorithm headerSegment = do
  { alg } <- (liftEffect $ parseHeader headerSegment) >>= throwLeftAsM (\_ -> throw { jwtError: DecodeError })
  algorithmFromString alg
  where
  parseHeader :: String -> Effect (Either MultipleErrors { alg :: String })
  parseHeader = (pure <<< readJSON) <=< base64URLDecode

algorithmFromString :: String -> Om {} (jwtError :: JwtError) Algorithm
algorithmFromString alg
  | alg == show HS256 = pure HS256
  | alg == show HS512 = pure HS512
  | otherwise = throw { jwtError: NotSupportedAlgorithmError }

base64URLDecode :: String -> Effect String
base64URLDecode x =
  (Buffer.fromString (unescape x) Base64 :: Effect Buffer) >>= Buffer.toString UTF8

unescape :: String -> String
unescape x =
  rep $ x <> (joinWith "=" $ replicate n "")
  where
  n = 5 - (length x) `mod` 4
  rep =
    replace (unsafeRegex "\\-" global) "+"
      >>> replace (unsafeRegex "_" global) "/"

-- | Encode to JWT.
encodeJwt :: forall payload. WriteForeign payload => Secret -> Algorithm -> payload -> Effect Jwt
encodeJwt secret alg payload = do
  headerSegment <- base64URLEncode $ writeJSON { typ: "JWT", alg: show alg }
  payloadSegment <- base64URLEncode $ writeJSON payload
  signatureSegment <- sign secret alg $ headerSegment <> "." <> payloadSegment
  pure $ Jwt $ headerSegment <> "." <> payloadSegment <> "." <> signatureSegment

sign :: Secret -> Algorithm -> String -> Effect String
sign secret alg input = do
  sec <- Buffer.fromString secret UTF8
  inp <- Buffer.fromString input UTF8
  result <- Hmac.createHmac (convertAlgorithm alg) sec >>= Hmac.update inp >>= Hmac.digest >>= Buffer.toString Base64
  pure $ escape result

base64URLEncode :: String -> Effect String
base64URLEncode x =
  escape <$> ((Buffer.fromString x UTF8 :: Effect Buffer) >>= Buffer.toString Base64)

escape :: String -> String
escape =
  replace (unsafeRegex "\\+" global) "-"
    >>> replace (unsafeRegex "\\/" global) "_"
    >>> replace (unsafeRegex "=" global) ""

convertAlgorithm :: Algorithm -> String
convertAlgorithm HS256 = "sha256"
convertAlgorithm HS512 = "sha512"
