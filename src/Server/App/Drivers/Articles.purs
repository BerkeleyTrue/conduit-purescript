module Server.App.Drivers.Articles
  ( ArticlesRoute(..)
  , articlesRoute
  , mkArticlesRouter
  ) where

import Prelude hiding ((/))

import Conduit.Data.Limit (Limit(..))
import Conduit.Data.MySlug (MySlug)
import Conduit.Data.MySlug as Slug
import Conduit.Data.Offset (Offset(..))
import Conduit.Data.UserId (AuthorId)
import Conduit.Data.Username (Username)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Foreign (MultipleErrors)
import HTTPurple (Method(..), Response, RouteDuplex', badRequest', forbidden, int, jsonHeaders, notFound, ok, ok', optional, params, prefix, segment, string, toString, sum, (/), (?))
import Justifill (justifill)
import Server.Core.Services.Articles (ArticleService(..), ArticleUpdateInput)
import Server.Core.Services.User (UserOutput)
import Server.Infra.Data.Route (limitR, offsetR, slugR, userIdR)
import Server.Infra.HttPurple.Types (OmRouter)
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
  | Comment MySlug Int
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
  , "Comment": slugR segment / "comment" / int segment
  , "Fav": slugR segment / "favorite"
  }

type ArticlesRouterExts ext = (user :: Maybe UserOutput | ext)

defaultErrorHandlers
  :: forall ctx errOut
   . Om ctx (articleRepoErr :: String, parsingErr :: MultipleErrors | errOut) Response
  -> Om ctx errOut Response
defaultErrorHandlers = handleErrors
  { articleRepoErr: \err -> badRequest' jsonHeaders $ writeJSON { message: show err }
  , parsingErr: \err -> badRequest' jsonHeaders $ writeJSON { message: show err }
  }

mkArticlesRouter :: forall ext. { articleService :: ArticleService } -> OmRouter ArticlesRoute (ArticlesRouterExts ext)
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
mkArticlesRouter { articleService: (ArticleService { getBySlug }) } { route: BySlug slug, method: Get } = defaultErrorHandlers do
  output <- expandErr $ getBySlug slug
  ok' jsonHeaders $ writeJSON output

-- Update Article by Slug
mkArticlesRouter { articleService: (ArticleService { update }) } { route: BySlug slug, method: Put, body, user } =
  case user of
    Nothing -> forbidden
    Just _ -> defaultErrorHandlers do
      str <- fromAff $ toString body
      parsed <- expandErr $ parseInputFromString str
      article <- expandErr $ update slug parsed.article
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
mkArticlesRouter _ { route: Comments slug, method: Get } = ok $ "get comments for" <> Slug.toString slug
-- update comments for article by slug
mkArticlesRouter _ { route: Comments slug, method: Post } = ok $ "post comments for" <> Slug.toString slug
mkArticlesRouter _ { route: Comments _ } = notFound

-- get comment by id for article by slug
mkArticlesRouter _ { route: Comment slug id, method: Delete } = ok $ "delete " <> (show id) <> " comment on " <> Slug.toString slug
mkArticlesRouter _ { route: Comment _ _ } = notFound

-- fav an article by slug
mkArticlesRouter _ { route: Fav slug, method: Get } = ok $ "fav an article " <> Slug.toString slug
-- unfav an article by slug
mkArticlesRouter _ { route: Fav slug, method: Delete } = ok $ "unfav an article " <> Slug.toString slug
mkArticlesRouter _ { route: Fav _ } = notFound
