module Server.Main (main) where

import Prelude

import Conduit.Data.Int (fromString')
import Data.Either (Either(..))
import Dotenv (loadFile)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Server.Infra (createApp)
import Server.Infra.Node.Process (lookupEnv')

main :: Effect Unit
main = launchAff_ do
  loadFile
  maybePort <- liftEffect $ lookupEnv' "PORT" <#> (_ >>= fromString')
  case maybePort of
    Right port -> createApp port
    Left err -> liftEffect $ throw err
