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
import Server.Infra (createApp)
import Server.Infra.Node.Process (lookupEnv_)

main :: Effect Unit
main = launchAff_ do
  loadFile
  maybePort <- liftEffect $ runExceptT $ lookupEnv_ "PORT" >>= fromString_
  case maybePort of
    Right port -> createApp port
    Left err -> liftEffect $ throw err
