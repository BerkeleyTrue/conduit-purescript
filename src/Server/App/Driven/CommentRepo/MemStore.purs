module Server.App.Driven.CommentRepo.MemStore
  ( mkCommentMemoryRepo
  ) where

import Prelude

import Conduit.Data.ArticleId (ArticleId)
import Conduit.Data.CommentId (CommentId, mkCommentId)
import Data.Array (filter, mapMaybe, singleton)
import Data.JSDate (now)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.AVar (AVar)
import Effect.Aff.AVar as Avar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Server.Core.Domain.Comment (Comment)
import Server.Core.Ports.Ports (CommentCreateInput, CommentRepo(..))
import Yoga.Om (Om, fromAff, note)

type CommentMap = Map CommentId Comment
type ArticleToCommentId = Map ArticleId (Array CommentId)

type MemStore =
  { byId :: CommentMap
  , byArticleId :: ArticleToCommentId
  }

mkCreate :: AVar MemStore -> CommentCreateInput -> Om {} (commentRepoErr :: String) Comment
mkCreate storeRef { body, authorId, articleId } = do
  { byId, byArticleId } <- liftAff $ Avar.take storeRef
  commentId <- liftEffect $ mkCommentId
  now <- liftEffect $ now

  let
    (newComment :: Comment) =
      { commentId
      , articleId
      , authorId
      , body
      , createdAt: now
      , updatedAt: Nothing
      }

    newStore =
      { byId: Map.insert commentId newComment byId
      , byArticleId: Map.insertWith (<>) articleId (singleton commentId) byArticleId
      }

  liftAff $ Avar.put newStore storeRef
  pure newComment

mkGetById :: AVar MemStore -> CommentId -> Om {} (commentRepoErr :: String) Comment
mkGetById storeRef commentId = do
  { byId } <- fromAff $ Avar.read storeRef
  note { commentRepoErr: "Comment for id " <> (show commentId) <> "not found." } $ Map.lookup commentId byId

mkGetByArticleId :: AVar MemStore -> ArticleId -> Om {} (commentRepoErr :: String) (Array Comment)
mkGetByArticleId storeRef articleId = do
  { byId, byArticleId } <- fromAff $ Avar.read storeRef
  note { commentRepoErr: "Comments for article id " <> (show articleId) <> "not found." } $ do
    commentIds <- Map.lookup articleId byArticleId
    pure $ mapMaybe (flip Map.lookup byId) commentIds

mkUpdate :: AVar MemStore -> CommentId -> (Comment -> Comment) -> Om {} (commentRepoErr :: String) Comment
mkUpdate storeRef commentId updateFn = do
  { byId, byArticleId } <- fromAff $ Avar.take storeRef
  comment <- note { commentRepoErr: "Comment for id " <> (show commentId) <> "not found." } $ Map.lookup commentId byId
  let newComment = updateFn comment
  liftAff $ Avar.put
    { byId: Map.insert commentId newComment byId
    , byArticleId
    }
    storeRef
  pure comment

mkDelete :: AVar MemStore -> CommentId -> Om {} (commentRepoErr :: String) Unit
mkDelete storeRef commentId = do
  { byId, byArticleId } <- fromAff $ Avar.take storeRef
  _ <- note { commentRepoErr: "Comment for id " <> (show commentId) <> " not found." } $ Map.lookup commentId byId

  let
    newStore =
      { byId: Map.delete commentId byId
      , byArticleId: Map.mapMaybe (Just <<< (filter (_ /= commentId))) byArticleId
      }

  liftAff $ Avar.put newStore storeRef
  pure unit

mkCommentMemoryRepo :: CommentMap -> Om {} () CommentRepo
mkCommentMemoryRepo initialComments = do
  storeRef <- fromAff $ Avar.new { byId: initialComments, byArticleId: Map.empty }
  pure $
    CommentRepo
      { create: mkCreate storeRef
      , getById: mkGetById storeRef
      , getByArticleId: mkGetByArticleId storeRef
      , update: mkUpdate storeRef
      , delete: mkDelete storeRef
      }
