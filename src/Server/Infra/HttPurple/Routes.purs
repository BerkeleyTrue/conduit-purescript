module Server.Infra.HttPurple.Routes
  ( omOrElse
  , (</>)
  ) where

import Prelude

import Data.Either (Either(..))
import HTTPurple (Request, type (<+>))
import Record as Record
import Server.Infra.HttPurple.Types (OmResponse)
import Type.Proxy (Proxy(..))

-- | Combine two OmRouters
omOrElse
  :: forall left right
   . (Request left -> OmResponse)
  -> (Request right -> OmResponse)
  -> Request (left <+> right)
  -> OmResponse
omOrElse leftRouter _ request@{ route: Left l } = leftRouter $ Record.set (Proxy :: _ "route") l request
omOrElse _ rightRouter request@{ route: Right r } = rightRouter $ Record.set (Proxy :: _ "route") r request

-- | Combine to OmRouters
infixr 3 omOrElse as </>
