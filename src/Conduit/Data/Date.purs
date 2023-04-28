module Conduit.Data.Date where

import Prelude

import Conduit.Control.Monad.Except (maybeThrow)
import Data.Date (Date)
import Data.JSDate (parse, toDate)
import Data.Maybe (fromMaybe')
import Effect.Class (liftEffect)
import Effect.Now (nowDate)
import Yoga.JSON (class ReadForeign, readImpl)

data MyDate = Date

instance ReadForeign MyDate where
  readImpl json = do
    str :: String <- readImpl json
    date <- liftEffect $ parse str
    maybeThrow "Can't read date" $ toDate date
