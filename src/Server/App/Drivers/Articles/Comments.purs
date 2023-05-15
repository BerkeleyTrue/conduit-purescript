module Server.App.Drivers.Articles.Comments where

import Prelude hiding ((/))

import Conduit.Data.CommentId (CommentId)
import Conduit.Data.MySlug (MySlug)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Foreign (MultipleErrors)
import HTTPurple
  ( Method(..)
  , Response
  , RouteDuplex'
  , badRequest'
  , forbidden
  , jsonHeaders
  , notFound
  , ok'
  , segment
  , toString
  , sum
  , (/)
  )
import Server.Core.Services.Articles (ArticleService(..))
import Server.Core.Services.Comment (CommentService(..), CommentServiceErrs)
import Server.Core.Services.User (UserService(..), UserOutput)
import Server.Infra.Data.Route (commentIdR, slugR)
import Server.Infra.HttPurple.Types (OmRouter)
import Type.Row (type (+))
import Yoga.JSON (writeJSON)
import Yoga.Om (Om, expandErr, fromAff, handleErrors)

data CommentRoute
  = Comments MySlug
  | Comment MySlug CommentId

derive instance genericCommentsRoute :: Generic CommentRoute _

commentRoute :: RouteDuplex' CommentRoute
commentRoute = sum
  { "Comments": (slugR segment) / "comments"
  , "Comment": (slugR segment) / "comments" / commentIdR segment
  }

defaultErrorHandlers
  :: forall ctx errOut
   . Om ctx (CommentServiceErrs + (parsingErr :: MultipleErrors | errOut)) Response
  -> Om ctx errOut Response
defaultErrorHandlers = handleErrors
  { articleRepoErr: \err -> badRequest' jsonHeaders $ writeJSON { message: show err }
  , commentRepoErr: \err -> badRequest' jsonHeaders $ writeJSON { message: show err }
  , parsingErr: \err -> badRequest' jsonHeaders $ writeJSON { message: show err }
  , userRepoErr: \err -> badRequest' jsonHeaders $ writeJSON { message: show err }
  }

type CommentRouterDeps r =
  ( articleService :: ArticleService
  , commentService :: CommentService
  , userService :: UserService
  | r
  )

type CommentRouteExt ext = (user :: Maybe UserOutput | ext)

mkCommentRouter :: forall ext r. { | CommentRouterDeps  r } -> OmRouter CommentRoute (CommentRouteExt ext)
mkCommentRouter
  { commentService: (CommentService { list }) }
  { route: Comments slug, method: Get } = defaultErrorHandlers do
  output <- expandErr $ list slug <#> writeJSON
  ok' jsonHeaders output

-- create comment
mkCommentRouter
  { userService: (UserService { getIdFromUsername })
  , articleService: (ArticleService { getIdFromSlug })
  , commentService: (CommentService { add })
  }
  { route: Comments slug, method: Post, body, user } = defaultErrorHandlers do
  case user of
    Nothing -> forbidden
    Just user' -> do
      authorId <- expandErr $ getIdFromUsername user'.username
      articleId <- expandErr $ getIdFromSlug slug
      body' <- fromAff $ toString body
      output <- expandErr $ add slug { articleId, body: body', authorId } <#> writeJSON
      ok' jsonHeaders output

mkCommentRouter _ { route: Comments _ } = notFound

-- delete comment by id for article by slug
mkCommentRouter { commentService: (CommentService { delete }) } { route: Comment _ id, method: Delete } = defaultErrorHandlers do
  expandErr $ delete id
  ok' jsonHeaders $ writeJSON { message: "Comment deleted" }

mkCommentRouter _ { route: Comment _ _ } = notFound
