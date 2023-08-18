module Server.Main (main) where

import Prelude

import Conduit.Data.Int (fromString_)
import Conduit.Data.UserId (UserId)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Map (Map)
import Dotenv (loadFile)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Foreign (MultipleErrors)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Server.Core.Domain.User (Email, User)
import Server.Infra (omApp)
import Server.Infra.Node.Process (lookupEnv_)
import Yoga.JSON (readJSON)
import Yoga.Om (runOm)

main :: Effect Unit
main = launchAff_ do
  loadFile
  file <- readTextFile UTF8 "./Server/seed.json"
  let res = readInitData file
  case res of
    Left err -> liftEffect $ throw $ show err
    Right initData -> do
      liftEffect $ log "Init data loaded"

      maybeEnvs <- liftEffect $ runExceptT do
        port <- lookupEnv_ "PORT" >>= fromString_
        tokenSecret <- lookupEnv_ "TOKEN_SECRET"
        pure { port, tokenSecret }

      case maybeEnvs of
        Left err -> liftEffect $ throw err

        Right envs -> do
          runOm envs { exception: \err -> liftEffect $ throw $ show err } omApp
  where
  readInitData :: String -> (Either MultipleErrors { user :: Map UserId User })
  readInitData = readJSON
