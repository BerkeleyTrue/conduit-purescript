module Server.Infra.HttPurple.Middleware.ParseUser where

import Prelude

import Conduit.Data.UserId (UserId)
import Data.Maybe (Maybe(..))
import Effect.Aff (message)
import Effect.Class (liftEffect)
import Effect.Console (error)
import HTTPurple (ResponseM, forbidden', internalServerError', jsonHeaders)
import Prim.Row (class Nub, class Union)
import Record (merge)
import Server.Core.Services.User (UserService(..), UserOutput)
import Yoga.JSON (writeJSON)
import Yoga.Om (fromAff, runOm)

mkParseUserMiddleware
  :: forall requestOut requestIn authExt t102 t103
   . Union requestOut (user :: Maybe UserOutput) t102
  => Nub (authed :: Maybe { userId :: UserId | authExt } | t102) requestIn
  => Nub (authed :: Maybe { userId :: UserId | authExt } | t103) requestIn
  => UserService
  -> (Record requestIn -> ResponseM)
  -> { authed :: Maybe { userId :: UserId | authExt } | requestOut }
  -> ResponseM
mkParseUserMiddleware (UserService { getUser }) router request@{ authed } = runOm
  {}
  { exception: \err -> do
      liftEffect $ error $ message err
      internalServerError' jsonHeaders $ writeJSON { message: "Opps, something went wrong" }
  , userRepoErr: \err -> do
      liftEffect $ error $ show err
      forbidden' jsonHeaders
  }
  case authed of
    Nothing -> do
      fromAff $ router $ merge request { user: Nothing :: Maybe UserOutput }
    Just { userId } -> do
      user <- getUser userId
      fromAff $ router $ merge request { user: Just user }
