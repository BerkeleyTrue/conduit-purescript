module Server.App.Api.Articles where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromMaybe)
import HTTPurple (Method(..), RouteDuplex', int, notFound, ok, optional, prefix, segment, string, sum, (/), (?))
import Server.Infra.HttPurple.Types (Router)

data ArticlesRoute
  = List -- Get
      { limit :: Maybe Int
      , offset :: Maybe Int
      , favorited :: Maybe String
      , author :: Maybe String
      , tag :: Maybe String
      }

  | Feed -- Get
      { limit :: Maybe Int
      , offset :: Maybe Int
      }
  | BySlug String -- get, put, delete
  | Comments String
  | Comment String Int
  | Fav String

derive instance genericArticlesRoute :: Generic ArticlesRoute _

articlesRoute :: RouteDuplex' ArticlesRoute
articlesRoute = prefix "articles" $ sum
  { "List": "" ?
      { limit: optional <<< int
      , offset: optional <<< int
      , favorited: optional <<< string
      , author: optional <<< string
      , tag: optional <<< string
      }
  , "Feed": "feed" ?
      { limit: optional <<< int
      , offset: optional <<< int
      }
  , "BySlug": string segment
  , "Comments": string segment / "comments"
  , "Comment": string segment / "comment" / int segment
  , "Fav": string segment / "favorite"
  }

articlesRouter :: Router ArticlesRoute
articlesRouter
  { route: List
      { limit, offset, favorited, author, tag }
  , method: Get
  } = ok $ "get " <> show limit'
  <> " articles after "
  <> show offset'
  <> " steps"
  <> (if (favorited' == "") then "" else "favorited by " <> favorited')
  <> (if (author' == "") then "" else "written by " <> author')
  <> (if (tag' == "") then "" else "with the following tag " <> tag')

  where
  limit' = fromMaybe 20 limit
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

articlesRouter { route: BySlug slug, method: Get } = ok $ "Get by Slug: " <> slug
articlesRouter { route: BySlug slug, method: Put } = ok $ "Update by Slug: " <> slug
articlesRouter { route: BySlug slug, method: Delete } = ok $ "Delete by Slug: " <> slug
articlesRouter { route: BySlug _ } = notFound

articlesRouter { route: Comments slug, method: Get } = ok $ "get comments for" <> slug
articlesRouter { route: Comments slug, method: Post } = ok $ "post comments for" <> slug
articlesRouter { route: Comments _ } = notFound

articlesRouter { route: Comment slug id, method: Delete } = ok $ "delete " <> (show id) <> " comment on " <> slug
articlesRouter { route: Comment _ _ } = notFound

articlesRouter { route: Fav slug, method: Get } = ok $ "fav an article " <> slug
articlesRouter { route: Fav slug, method: Delete } = ok $ "unfav an article " <> slug
articlesRouter { route: Fav _ } = notFound
