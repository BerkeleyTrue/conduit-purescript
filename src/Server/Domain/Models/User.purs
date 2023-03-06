module Server.Domain.Models.User (User(..)) where

import Data.Maybe (Maybe)

data User = User
  { email :: String
  , username :: String
  , bio :: String
  , token :: Maybe String
  , image :: Maybe String
  }
