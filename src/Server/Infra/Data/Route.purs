module Server.Infra.Data.Route
  ( limitR
  , slugR
  , usernameR
  , offsetR
  , userIdR
  , authorIdR
  , commentIdR
  , articleIdR
  ) where

import Prelude

import Conduit.Data.ArticleId (ArticleId, articleIdFromString, articleIdToString)
import Conduit.Data.CommentId (CommentId, commentIdFromString, commentIdToString)
import Conduit.Data.Limit (Limit, limitToString, stringToLimit)
import Conduit.Data.MySlug (MySlug, parse, toString)
import Conduit.Data.Offset (Offset, offsetToString, stringToOffset)
import Conduit.Data.UserId (AuthorId, UserId, userIdFromString, userIdToString)
import Conduit.Data.Username (Username, mkUsername)
import Data.Bifunctor (lmap)
import Data.Either (note)
import HTTPurple (RouteDuplex', as)

slugR :: RouteDuplex' String -> RouteDuplex' MySlug
slugR = as toString (parse >>> note "Invalid slug")

limitR :: RouteDuplex' String -> RouteDuplex' Limit
limitR = as limitToString stringToLimit

offsetR :: RouteDuplex' String -> RouteDuplex' Offset
offsetR = as offsetToString stringToOffset

usernameR :: RouteDuplex' String -> RouteDuplex' Username
usernameR = as show (lmap show <<< mkUsername)

userIdR :: RouteDuplex' String -> RouteDuplex' UserId
userIdR = as userIdToString userIdFromString

authorIdR :: RouteDuplex' String -> RouteDuplex' AuthorId
authorIdR = as userIdToString userIdFromString

articleIdR :: RouteDuplex' String -> RouteDuplex' ArticleId
articleIdR = as articleIdToString articleIdFromString

commentIdR :: RouteDuplex' String -> RouteDuplex' CommentId
commentIdR = as commentIdToString commentIdFromString
