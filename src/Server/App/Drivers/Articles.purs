module Server.App.Drivers.Articles
  ( ArticlesRoute(..)
  , articlesRoute
  , mkArticlesRouter
  ) where

import Prelude hiding ((/))

import Conduit.Data.CommentId (CommentId)
import Conduit.Data.Limit (Limit(..))
import Conduit.Data.MySlug (MySlug)
import Conduit.Data.Offset (Offset(..))
import Conduit.Data.UserId (AuthorId)
import Conduit.Data.Username (Username)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Foreign (MultipleErrors)
import HTTPurple (Method(..), Response, RouteDuplex', badRequest', forbidden, jsonHeaders, notFound, ok', optional, params, prefix, segment, string, toString, sum, (/), (?))
import Justifill (justifill)
import Server.Core.Services.Articles (ArticleService(..), ArticleUpdateInput)
import Server.Core.Services.Comment (CommentService(..), CommentServiceErrs)
import Server.Core.Services.User (UserOutput, UserService(..))
import Server.Infra.Data.Route (commentIdR, limitR, offsetR, slugR, userIdR)
import Server.Infra.HttPurple.Types (OmRouter)
import Type.Row (type (+))
import Yoga.JSON (readJSON, writeJSON)
import Yoga.Om (Om, expandErr, fromAff, handleErrors, throw, throwLeftAsM)

data ArticlesRoute
  = List -- Get
      { limit :: Maybe Limit
      , offset :: Maybe Offset
      , favorited :: Maybe AuthorId -- favorited by
      , author :: Maybe AuthorId -- written by
      , tag :: Maybe String
      }
  | Feed -- Get
      { limit :: Maybe Limit
      , offset :: Maybe Offset
      }
  | BySlug MySlug -- get, put, delete
  | Comments MySlug
  | Comment MySlug CommentId
  | Fav MySlug

derive instance genericArticlesRoute :: Generic ArticlesRoute _

articlesRoute :: RouteDuplex' ArticlesRoute
articlesRoute = prefix "articles" $ sum
  { "List": params
      { limit: optional <<< limitR
      , offset: optional <<< offsetR
      , favorited: optional <<< userIdR
      , author: optional <<< userIdR
      , tag: optional <<< string
      }
  , "Feed": "feed" ?
      { limit: optional <<< limitR
      , offset: optional <<< offsetR
      }
  , "BySlug": slugR segment
  , "Comments": slugR segment / "comments"
  , "Comment": slugR segment / "comment" / commentIdR segment
  , "Fav": slugR segment / "favorite"
  }

type ArticlesRouterExts ext = (user :: Maybe UserOutput | ext)
type ArticlesRouterDeps = { articleService :: ArticleService, commentService :: CommentService, userService :: UserService }

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

mkArticlesRouter :: forall ext. ArticlesRouterDeps -> OmRouter ArticlesRoute (ArticlesRouterExts ext)
mkArticlesRouter { articleService: (ArticleService { list }) } { route: List { limit, offset, favorited, author, tag }, method: Get, user } = defaultErrorHandlers do
  let (username :: Maybe Username) = user <#> _.username
  output <- expandErr $ list { username, input }
  ok' jsonHeaders $ writeJSON output

  where
  limit' = fromMaybe (Limit 20) limit
  author' = author
  offset' = fromMaybe (Offset 0) offset
  input = justifill
    { limit: limit'
    , author: author'
    , offset: offset'
    , favorited
    , tag
    }

mkArticlesRouter _ { route: List _ } = notFound

mkArticlesRouter { articleService: (ArticleService { list }) } { route: Feed { limit, offset }, method: Get, user } = defaultErrorHandlers do
  case user <#> _.username of
    Nothing -> forbidden
    Just username -> do
      output <- expandErr $ list { username: Just username, input }
      ok' jsonHeaders $ writeJSON output

  where
  limit' = fromMaybe (Limit 20) limit
  offset' = fromMaybe (Offset 0) offset
  input = justifill
    { limit: limit'
    , offset: offset'
    }

mkArticlesRouter _ { route: Feed _ } = notFound

-- Get Article by Slug
mkArticlesRouter { articleService: (ArticleService { getBySlug }) } { route: BySlug slug, method: Get, user } = defaultErrorHandlers do
  output <- expandErr $ getBySlug slug $ user <#> _.username
  ok' jsonHeaders $ writeJSON output

-- Update Article by Slug
mkArticlesRouter { articleService: (ArticleService { update }) } { route: BySlug slug, method: Put, body, user } =
  case user of
    Nothing -> forbidden
    Just user' -> defaultErrorHandlers do
      str <- fromAff $ toString body
      parsed <- expandErr $ parseInputFromString str
      article <- expandErr $ update slug user'.username parsed.article
      ok' jsonHeaders $ writeJSON article

  where
  parseInputFromString :: String -> Om {} (parsingErr :: MultipleErrors) { article :: ArticleUpdateInput }
  parseInputFromString = throwLeftAsM (\err -> throw { parsingErr: err }) <<< readJSON

-- Delete Article by Slug
mkArticlesRouter { articleService: (ArticleService { delete }) } { route: BySlug slug, method: Delete, user } =
  case user of
    Nothing -> forbidden
    Just _ -> defaultErrorHandlers do
      expandErr $ delete slug
      ok' jsonHeaders $ writeJSON { message: "Article deleted" }

mkArticlesRouter _ { route: BySlug _ } = notFound

-- get comments for article by slug
mkArticlesRouter { commentService: (CommentService { list }) } { route: Comments slug, method: Get } = defaultErrorHandlers do
  output <- expandErr $ list slug <#> writeJSON
  ok' jsonHeaders output

-- create comment
mkArticlesRouter
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

mkArticlesRouter _ { route: Comments _ } = notFound

-- delete comment by id for article by slug
mkArticlesRouter { commentService: (CommentService { delete }) } { route: Comment _ id, method: Delete } = defaultErrorHandlers do
  expandErr $ delete id
  ok' jsonHeaders $ writeJSON { message: "Comment deleted" }

mkArticlesRouter _ { route: Comment _ _ } = notFound

-- fav an article by slug
mkArticlesRouter { articleService: (ArticleService { favorite }) } { route: Fav slug, method: Get, user } = defaultErrorHandlers do
  case user of
    Nothing -> forbidden
    Just user' -> do
      output <- expandErr $ favorite slug user'.username <#> writeJSON
      ok' jsonHeaders output

-- unfav an article by slug
mkArticlesRouter { articleService: (ArticleService { unfavorite }) } { route: Fav slug, method: Delete, user } = defaultErrorHandlers do
  case user of
    Nothing -> forbidden
    Just user' -> do
      output <- expandErr $ unfavorite slug user'.username <#> writeJSON
      ok' jsonHeaders output

mkArticlesRouter _ { route: Fav _ } = notFound
