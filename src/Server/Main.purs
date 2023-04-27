module Server.Main (main) where

import Prelude

import Conduit.Data.Int (fromString_)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Dotenv (loadFile)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Server.Infra (omApp)
import Server.Infra.Node.Process (lookupEnv_)
import Yoga.Om (runOm)

main :: Effect Unit
main = launchAff_ do
  loadFile
  maybeEnvs <- liftEffect $ runExceptT do
    port <- lookupEnv_ "PORT" >>= fromString_
    tokenSecret <- lookupEnv_ "TOKEN_SECRET"
    pure { port, tokenSecret }
  case maybeEnvs of
    Right envs -> runOm
      envs
      { exception: \err -> liftEffect $ throw $ show err }
      omApp
    Left err -> liftEffect $ throw err
