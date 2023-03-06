module Server.App.Api.Articles where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromMaybe)
import HTTPurple (Method(..), RouteDuplex', int, notFound, ok, optional, params, prefix, segment, string, sum, (/), (?))
import Server.Infra.Data.Route as Route
import Server.Infra.HttPurple.Types (Router)
import Slug (Slug)
import Slug as Slug

data ArticlesRoute
  = List -- Get
      { limit :: Maybe Route.Limit
      , offset :: Maybe Int
      , favorited :: Maybe String -- favorited by
      , author :: Maybe String -- written by
      , tag :: Maybe String
      }
  | Feed -- Get
      { limit :: Maybe Int
      , offset :: Maybe Int
      }
  | BySlug Slug -- get, put, delete
  | Comments Slug
  | Comment Slug Int
  | Fav Slug

derive instance genericArticlesRoute :: Generic ArticlesRoute _

articlesRoute :: RouteDuplex' ArticlesRoute
articlesRoute = prefix "articles" $ sum
  { "List": params
      { limit: optional <<< Route.limit
      , offset: optional <<< int
      , favorited: optional <<< string
      , author: optional <<< string
      , tag: optional <<< string
      }
  , "Feed": "feed" ?
      { limit: optional <<< int
      , offset: optional <<< int
      }
  , "BySlug": Route.slug segment
  , "Comments": Route.slug segment / "comments"
  , "Comment": Route.slug segment / "comment" / int segment
  , "Fav": Route.slug segment / "favorite"
  }

articlesRouter :: Router ArticlesRoute
articlesRouter
  { route: List
      { limit
      , offset
      , favorited
      , author
      , tag
      }
  , method: Get
  } = ok $ "get " <> show limit'
  <> " articles after "
  <> show offset'
  <> " steps"
  <> (if (favorited' == "") then "" else "favorited by " <> favorited')
  <> (if (author' == "") then "" else "written by " <> author')
  <> (if (tag' == "") then "" else "with the following tag " <> tag')

  where
  Route.Limit limit' = fromMaybe (Route.Limit 20) limit -- rethrow if query is out of bounds?
  offset' = fromMaybe 0 offset
  favorited' = fromMaybe "" favorited
  author' = fromMaybe "" author
  tag' = fromMaybe "" tag

articlesRouter { route: List _ } = notFound

articlesRouter { route: Feed { limit, offset }, method: Get } = ok
  $ "feed me "
      <> show limit'
      <> " articles offset by "
      <> show offset'
      <> " offset"

  where
  limit' = fromMaybe 20 limit
  offset' = fromMaybe 0 offset
articlesRouter { route: Feed _ } = notFound

articlesRouter { route: BySlug slug, method: Get } = ok $ "Get by Slug: " <> Slug.toString slug
articlesRouter { route: BySlug slug, method: Put } = ok $ "Update by Slug: " <> Slug.toString slug
articlesRouter { route: BySlug slug, method: Delete } = ok $ "Delete by Slug: " <> Slug.toString slug
articlesRouter { route: BySlug _ } = notFound

articlesRouter { route: Comments slug, method: Get } = ok $ "get comments for" <> Slug.toString slug
articlesRouter { route: Comments slug, method: Post } = ok $ "post comments for" <> Slug.toString slug
articlesRouter { route: Comments _ } = notFound

articlesRouter { route: Comment slug id, method: Delete } = ok $ "delete " <> (show id) <> " comment on " <> Slug.toString slug
articlesRouter { route: Comment _ _ } = notFound

articlesRouter { route: Fav slug, method: Get } = ok $ "fav an article " <> Slug.toString slug
articlesRouter { route: Fav slug, method: Delete } = ok $ "unfav an article " <> Slug.toString slug
articlesRouter { route: Fav _ } = notFound
