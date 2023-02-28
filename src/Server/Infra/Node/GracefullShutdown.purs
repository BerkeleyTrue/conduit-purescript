module Server.Infra.Node.GracefullShutdown where

import Prelude

import Data.Posix.Signal (Signal(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Process (onSignal, onUncaughtException)

-- TODO: add way to add shutdown handlers


gracefullShutdown :: (Effect Unit -> Effect Unit) -> Effect Unit
gracefullShutdown shutdown = do
  let
    shutdownHandler = do
      log "Shutting down server..."
      shutdown $ log "Server shutdown"

  onSignal SIGINT shutdownHandler
  onSignal SIGTERM shutdownHandler
  onSignal SIGUSR2 shutdownHandler -- nodemon
  onUncaughtException \err -> do
    _ <- log $ "Uncaught exception: " <> show err
    shutdownHandler
