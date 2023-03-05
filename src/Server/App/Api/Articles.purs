module Server.App.Api.Articles where

import Prelude hiding ((/))

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe, fromMaybe, maybe)
import HTTPurple (Method(..), RouteDuplex', int, notFound, ok, optional, params, prefix, segment, string, sum, (/), (?), as)
import Server.Infra.HttPurple.Types (Router)

newtype Limit = Limit Int

derive instance genericLimit :: Generic Limit _

limitToString :: Limit -> String
limitToString (Limit x) = show x

stringToLimit :: String -> Either String Limit
stringToLimit val = case (maybe (Left "Could not parse") Right <<< Int.fromString) val  of
  Right l | l > 0 && l < 1000 -> Right $ Limit l
  Right _ -> Left "Limit out of bounds"
  Left mess -> Left mess



limitComb :: RouteDuplex' String -> RouteDuplex' Limit
limitComb = as limitToString stringToLimit

data ArticlesRoute
  = List -- Get
      { limit :: Maybe Limit
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
  { "List": params
      { limit: optional <<< limitComb
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
  Limit limit' = fromMaybe (Limit 20) limit -- rethrow if query is out of bounds?
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
