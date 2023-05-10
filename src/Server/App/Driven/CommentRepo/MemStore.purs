module Server.App.Driven.CommentRepo.MemStore where

import Prelude

import Conduit.Control.Monad.Except (maybeThrow)
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
import Server.Core.Domain.Article (ArticleId)
import Server.Core.Domain.Comment (Comment(..), CommentId(..))
import Server.Core.Ports.Ports (CommentCreateInput, CommentRepo(..))

type CommentMap = Map CommentId Comment
type ArticleToCommentId = Map ArticleId (Array CommentId)

type MemStore =
  { byId :: CommentMap
  , byArticleId :: ArticleToCommentId
  }

createComment :: AVar MemStore -> CommentCreateInput -> Aff (Either String Comment)
createComment storeRef { body, authorId, articleId } = runExceptT do
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

getCommentById :: AVar MemStore -> CommentId -> Aff (Either String Comment)
getCommentById storeRef commentId = runExceptT do
  { byId } <- liftAff $ Avar.take storeRef
  maybeThrow ("Comment for id " <> (show commentId) <> "not found.") $ Map.lookup commentId byId

getCommentsByArticleId :: AVar MemStore -> ArticleId -> Aff (Either String (Array Comment))
getCommentsByArticleId storeRef articleId = runExceptT do
  { byId, byArticleId } <- liftAff $ Avar.take storeRef
  maybeThrow ("Comments for article id " <> (show articleId) <> "not found.") $ do
    commentIds <- Map.lookup articleId byArticleId
    pure $ mapMaybe (flip Map.lookup byId) commentIds

listComments :: AVar MemStore -> Aff (Array Comment)
listComments storeRef = toUnfoldable <$> Map.values <$> _.byId <$> Avar.take storeRef

updateComment :: AVar MemStore -> CommentId -> (Comment -> Comment) -> Aff (Either String Comment)
updateComment storeRef commentId updateFn = runExceptT do
  { byId, byArticleId } <- liftAff $ Avar.take storeRef
  comment <- maybeThrow ("Comment for id " <> (show commentId) <> "not found.") $ Map.lookup commentId byId
  let newComment = updateFn comment
  liftAff $ Avar.put
    { byId: Map.insert commentId newComment byId
    , byArticleId
    } storeRef
  pure comment

deleteComment :: AVar MemStore -> CommentId -> Aff (Either String Unit)
deleteComment storeRef commentId = runExceptT do
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
      { create: createComment storeRef
      , getById: getCommentById storeRef
      , getByArticleId: getCommentsByArticleId storeRef
      , list: listComments storeRef
      , update: updateComment storeRef
      , delete: deleteComment storeRef
      }
