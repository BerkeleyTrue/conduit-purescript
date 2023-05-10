module Server.App.Drivers.Articles
  ( ArticlesRoute(..)
  , articlesRoute
  , mkArticlesRouter
  ) where

import Prelude hiding ((/))

import Conduit.Data.Limit (Limit(..))
import Conduit.Data.Offset (Offset(..))
import Conduit.Data.UserId (AuthorId)
import Conduit.Data.Username (Username)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromMaybe)
import HTTPurple
  ( Method(..)
  , Response
  , RouteDuplex'
  , badRequest'
  , int
  , jsonHeaders
  , notFound
  , ok
  , ok'
  , optional
  , params
  , prefix
  , segment
  , string
  , sum
  , (/)
  , (?)
  )
import Justifill (justifill)
import Server.Core.Services.Articles (ArticleService(..))
import Server.Core.Services.User (UserOutput)
import Server.Infra.Data.Route (limitR, offsetR, slugR, userIdR)
import Server.Infra.HttPurple.Types (OmRouter)
import Slug (Slug)
import Slug as Slug
import Yoga.JSON (writeJSON)
import Yoga.Om (Om, handleErrors)

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
  | BySlug Slug -- get, put, delete
  | Comments Slug
  | Comment Slug Int
  | Fav Slug

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
   . Om ctx (articleRepoErr :: String | errOut) Response
  -> Om ctx errOut Response
defaultErrorHandlers = handleErrors
  { articleRepoErr: \err -> badRequest' jsonHeaders $ writeJSON { message: show err }
  }

mkArticlesRouter :: forall ext. { articleService :: ArticleService } -> OmRouter ArticlesRoute (ArticlesRouterExts ext)
mkArticlesRouter { articleService: (ArticleService { list }) } { route: List { limit, offset, favorited, author, tag }, method: Get, user } = defaultErrorHandlers do
  let (username :: Maybe Username) = user <#> _.username
  output <- list { username, input }
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

mkArticlesRouter _ { route: Feed { limit, offset }, method: Get } = ok
  $ "feed me "
      <> show limit'
      <> " articles offset by "
      <> show offset'
      <> " offset"

  where
  limit' = fromMaybe (Limit 20) limit
  offset' = fromMaybe (Offset 0) offset

mkArticlesRouter _ { route: Feed _ } = notFound

mkArticlesRouter _ { route: BySlug slug, method: Get } = ok $ "Get by Slug: " <> Slug.toString slug
mkArticlesRouter _ { route: BySlug slug, method: Put } = ok $ "Update by Slug: " <> Slug.toString slug
mkArticlesRouter _ { route: BySlug slug, method: Delete } = ok $ "Delete by Slug: " <> Slug.toString slug
mkArticlesRouter _ { route: BySlug _ } = notFound

mkArticlesRouter _ { route: Comments slug, method: Get } = ok $ "get comments for" <> Slug.toString slug
mkArticlesRouter _ { route: Comments slug, method: Post } = ok $ "post comments for" <> Slug.toString slug
mkArticlesRouter _ { route: Comments _ } = notFound

mkArticlesRouter _ { route: Comment slug id, method: Delete } = ok $ "delete " <> (show id) <> " comment on " <> Slug.toString slug
mkArticlesRouter _ { route: Comment _ _ } = notFound

mkArticlesRouter _ { route: Fav slug, method: Get } = ok $ "fav an article " <> Slug.toString slug
mkArticlesRouter _ { route: Fav slug, method: Delete } = ok $ "unfav an article " <> Slug.toString slug
mkArticlesRouter _ { route: Fav _ } = notFound
