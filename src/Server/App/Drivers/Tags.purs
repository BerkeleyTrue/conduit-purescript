module Server.App.Drivers.Tags (TagsRoute(..), tagsRoute, mkTagsRouter) where

import Prelude

import Data.Generic.Rep (class Generic)
import HTTPurple (Method(..), Response, RouteDuplex', badRequest', jsonHeaders, noArgs, notFound, ok', prefix, sum)
import Server.Core.Services.Tags (TagService(..), TagServiceErrs)
import Server.Infra.HttPurple.Types (OmRouter)
import Yoga.JSON (writeJSON)
import Yoga.Om (Om, handleErrors)

data TagsRoute = List

derive instance genericTagRoute :: Generic TagsRoute _

tagsRoute :: RouteDuplex' TagsRoute
tagsRoute = prefix "tags" $ sum { "List": noArgs }

defaultErrorHandlers
  :: forall ctx errOut
   . Om ctx (TagServiceErrs errOut) Response
  -> Om ctx errOut Response
defaultErrorHandlers = handleErrors
  { articleRepoErr: \err -> badRequest' jsonHeaders $ writeJSON { message: show err }
  , userRepoErr: \err -> badRequest' jsonHeaders $ writeJSON { message: show err }
  }

mkTagsRouter :: forall ext. { tagService :: TagService } -> OmRouter TagsRoute ext
mkTagsRouter { tagService: (TagService { list }) } { route: List, method: Get } = defaultErrorHandlers $ do
  tags <- list
  ok' jsonHeaders $ writeJSON { tags }

mkTagsRouter _ { route: List } = notFound
