module Server.Domain.Models.Profile (Profile(..)) where

import Data.Maybe (Maybe)

data Profile = Profile
  { username :: String
  , bio :: Maybe String
  , image :: Maybe String
  , following :: Boolean
  }
