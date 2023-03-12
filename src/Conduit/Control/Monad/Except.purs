module Conduit.Control.Monad.Except where

import Prelude

import Control.Monad.Except (ExceptT, except)
import Data.Either (note)
import Data.Maybe (Maybe)

maybeThrow :: forall m e a. Monad m => e -> Maybe a -> ExceptT e m a
maybeThrow = (except <<< _) <<< note
