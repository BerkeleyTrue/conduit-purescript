module Server.App.Driven.CommentRepo.MemStore
  ( mkCommentMemoryRepo
  ) where

import Prelude

import Conduit.Control.Monad.Except (maybeThrow)
import Conduit.Data.ArticleId (ArticleId)
import Control.Monad.Except (runExceptT)
import Data.Array (filter, mapMaybe, singleton)
import Data.Either (Either)
import Data.JSDate (now)
import Data.List (toUnfoldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.UUID (genUUID)
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Effect.Aff.AVar as Avar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Server.Core.Domain.Comment (Comment(..), CommentId(..))
import Server.Core.Ports.Ports (CommentCreateInput, CommentRepo(..))

type CommentMap = Map CommentId Comment
type ArticleToCommentId = Map ArticleId (Array CommentId)

type MemStore =
  { byId :: CommentMap
  , byArticleId :: ArticleToCommentId
  }

mkCreate :: AVar MemStore -> CommentCreateInput -> Aff (Either String Comment)
mkCreate storeRef { body, authorId, articleId } = runExceptT do
  { byId, byArticleId } <- liftAff $ Avar.take storeRef
  commentId <- liftEffect $ CommentId <$> genUUID
  now <- liftEffect $ now

  let
    (newComment :: Comment) =
      Comment
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

mkGetById :: AVar MemStore -> CommentId -> Aff (Either String Comment)
mkGetById storeRef commentId = runExceptT do
  { byId } <- liftAff $ Avar.take storeRef
  maybeThrow ("Comment for id " <> (show commentId) <> "not found.") $ Map.lookup commentId byId

mkGetByArticleId :: AVar MemStore -> ArticleId -> Aff (Either String (Array Comment))
mkGetByArticleId storeRef articleId = runExceptT do
  { byId, byArticleId } <- liftAff $ Avar.take storeRef
  maybeThrow ("Comments for article id " <> (show articleId) <> "not found.") $ do
    commentIds <- Map.lookup articleId byArticleId
    pure $ mapMaybe (flip Map.lookup byId) commentIds

mkList :: AVar MemStore -> Aff (Array Comment)
mkList storeRef = toUnfoldable <$> Map.values <$> _.byId <$> Avar.take storeRef

mkUpdate :: AVar MemStore -> CommentId -> (Comment -> Comment) -> Aff (Either String Comment)
mkUpdate storeRef commentId updateFn = runExceptT do
  { byId, byArticleId } <- liftAff $ Avar.take storeRef
  comment <- maybeThrow ("Comment for id " <> (show commentId) <> "not found.") $ Map.lookup commentId byId
  let newComment = updateFn comment
  liftAff $ Avar.put
    { byId: Map.insert commentId newComment byId
    , byArticleId
    }
    storeRef
  pure comment

mkDelete :: AVar MemStore -> CommentId -> Aff (Either String Unit)
mkDelete storeRef commentId = runExceptT do
  { byId, byArticleId } <- liftAff $ Avar.take storeRef
  _ <- maybeThrow ("Comment for id " <> (show commentId) <> " not found.") $ Map.lookup commentId byId

  let
    newStore =
      { byId: Map.delete commentId byId
      , byArticleId: Map.mapMaybe (Just <<< (filter (_ /= commentId))) byArticleId
      }

  liftAff $ Avar.put newStore storeRef
  pure unit

mkCommentMemoryRepo :: CommentMap -> Aff (CommentRepo Aff)
mkCommentMemoryRepo initialComments = do
  storeRef <- Avar.new { byId: initialComments, byArticleId: Map.empty }
  pure $
    CommentRepo
      { create: mkCreate storeRef
      , getById: mkGetById storeRef
      , getByArticleId: mkGetByArticleId storeRef
      , list: mkList storeRef
      , update: mkUpdate storeRef
      , delete: mkDelete storeRef
      }
