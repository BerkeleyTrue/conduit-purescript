module Server.App.Drivers.Articles
  ( ArticlesRoute(..)
  , articlesRoute
  , ArticleRoute(..)
  , articleRoute
  , mkArticlesRouter
  ) where

import Prelude hiding ((/))

import Conduit.Data.Limit (Limit(..))
import Conduit.Data.MySlug (MySlug)
import Conduit.Data.Offset (Offset(..))
import Conduit.Data.UserId (AuthorId)
import Conduit.Data.Username (Username)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Foreign (MultipleErrors)
import HTTPurple (Method(..), Response, RouteDuplex', badRequest', forbidden, jsonHeaders, notFound, ok', optional, params, prefix, segment, string, toString, sum, (/), (?), type (<+>), (<+>))
import Justifill (justifill)
import Server.App.Drivers.Articles.Comments (CommentRoute, commentRoute, mkCommentRouter)
import Server.Core.Services.Articles (ArticleService(..), ArticleUpdateInput)
import Server.Core.Services.Comment (CommentService, CommentServiceErrs)
import Server.Core.Services.User (UserOutput, UserService)
import Server.Infra.Data.Route (limitR, offsetR, slugR, userIdR)
import Server.Infra.HttPurple.Routes ((</>))
import Server.Infra.HttPurple.Types (OmRouter)
import Type.Row (type (+))
import Yoga.JSON (readJSON, writeJSON)
import Yoga.Om (Om, expandErr, fromAff, handleErrors, throw, throwLeftAsM)

data ArticleRoute
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
  | Fav MySlug

derive instance genericArticleRoute :: Generic ArticleRoute _

articleRoute :: RouteDuplex' ArticleRoute
articleRoute = prefix "articles" $ sum
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
  , "Fav": slugR segment / "favorite"
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

type ArticleRouterExts ext = (user :: Maybe UserOutput | ext)
type ArticleRouterDeps r =
  ( articleService :: ArticleService
  , userService :: UserService
  | r
  )

mkArticleRouter :: forall ext r. { | ArticleRouterDeps r } -> OmRouter ArticleRoute (ArticleRouterExts ext)
mkArticleRouter { articleService: (ArticleService { list }) } { route: List { limit, offset, favorited, author, tag }, method: Get, user } = defaultErrorHandlers do
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

mkArticleRouter _ { route: List _ } = notFound

mkArticleRouter { articleService: (ArticleService { list }) } { route: Feed { limit, offset }, method: Get, user } = defaultErrorHandlers do
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

mkArticleRouter _ { route: Feed _ } = notFound

-- Get Article by Slug
mkArticleRouter { articleService: (ArticleService { getBySlug }) } { route: BySlug slug, method: Get, user } = defaultErrorHandlers do
  output <- expandErr $ getBySlug slug $ user <#> _.username
  ok' jsonHeaders $ writeJSON output

-- Update Article by Slug
mkArticleRouter { articleService: (ArticleService { update }) } { route: BySlug slug, method: Put, body, user } =
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
mkArticleRouter { articleService: (ArticleService { delete }) } { route: BySlug slug, method: Delete, user } =
  case user of
    Nothing -> forbidden
    Just _ -> defaultErrorHandlers do
      expandErr $ delete slug
      ok' jsonHeaders $ writeJSON { message: "Article deleted" }

mkArticleRouter _ { route: BySlug _ } = notFound

-- fav an article by slug
mkArticleRouter { articleService: (ArticleService { favorite }) } { route: Fav slug, method: Get, user } = defaultErrorHandlers do
  case user of
    Nothing -> forbidden
    Just user' -> do
      output <- expandErr $ favorite slug user'.username <#> writeJSON
      ok' jsonHeaders output

-- unfav an article by slug
mkArticleRouter { articleService: (ArticleService { unfavorite }) } { route: Fav slug, method: Delete, user } = defaultErrorHandlers do
  case user of
    Nothing -> forbidden
    Just user' -> do
      output <- expandErr $ unfavorite slug user'.username <#> writeJSON
      ok' jsonHeaders output

mkArticleRouter _ { route: Fav _ } = notFound

type ArticlesRoute = ArticleRoute <+> CommentRoute

articlesRoute :: RouteDuplex' ArticlesRoute
articlesRoute = articleRoute <+> commentRoute

mkArticlesRouter
  :: forall ext
   . { articleService :: ArticleService
     , userService :: UserService
     , commentService :: CommentService
     }
  -> OmRouter ArticlesRoute (ArticleRouterExts ext)
mkArticlesRouter deps = mkArticleRouter deps </> mkCommentRouter deps
