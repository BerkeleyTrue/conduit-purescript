module Server.Infra.HttPurple.Routes
  ( omOrElse
  , (</>)
  ) where

import Prelude

import Data.Either (Either(..))
import HTTPurple (type (<+>))
import Record as Record
import Server.Infra.HttPurple.Types (OmRouter)
import Type.Proxy (Proxy(..))

-- | Combine two OmRouters
omOrElse
  :: forall left right ext
   . (OmRouter left ext)
  -> (OmRouter right ext)
  -> OmRouter (left <+> right) ext
omOrElse leftRouter _ request@{ route: Left l } = leftRouter $ Record.set (Proxy :: _ "route") l request
omOrElse _ rightRouter request@{ route: Right r } = rightRouter $ Record.set (Proxy :: _ "route") r request

-- | Combine to OmRouters
infixr 3 omOrElse as </>
