module Server.App.Driven.ArticleRepo.MemStore
  ( mkMemoryArticleStore
  ) where

import Prelude

import Conduit.Control.Monad.Except (maybeThrow)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.UUID (genUUID)
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Effect.Aff.AVar as Avar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Now (nowDate)
import Server.Core.Domain.Article (Article(..), ArticleId(..))
import Server.Core.Ports.Ports (ArticleCreateInput, ArticleRepo(..))
import Slug (Slug, generate)

type ArticleMap = Map.Map ArticleId Article
type SlugToArticle = Map.Map Slug ArticleId

type MemStore =
  { byId :: ArticleMap
  , slugToId :: SlugToArticle
  }

createArticle :: AVar MemStore -> ArticleCreateInput -> Aff (Either String Article)
createArticle storeRef { title, description, body, tagList, authorId } = runExceptT do

  { byId, slugToId } <- liftAff $ Avar.take storeRef

  slug <- case generate title of
    Just slug -> do
      if Map.member slug slugToId then
        throwError $ "Expected unique slug but found a duplicate for " <> (show slug)
      else
        pure slug
    Nothing -> throwError $ "Expected a slug-able title but could not generate from " <> title

  articleId <- liftEffect $ ArticleId <$> genUUID
  now <- liftEffect $ nowDate

  let
    article = Article
      { articleId
      , title
      , description
      , body
      , slug
      , authorId
      , favoritesCount: 0
      , tagList: fromMaybe Nil tagList
      , createdAt: now
      , updatedAt: Nothing
      }
    newStore =
      { byId: Map.insert articleId article byId
      , slugToId: Map.insert slug articleId slugToId
      }

  liftAff $ Avar.put newStore storeRef
  pure article

getArticleById :: AVar MemStore -> ArticleId -> Aff (Either String Article)
getArticleById storeRef articleId = runExceptT do
  { byId } <- liftAff $ Avar.take storeRef
  maybeThrow ("Could not find article with id " <> show articleId) $ Map.lookup articleId byId

getArticleBySlug :: AVar MemStore -> Slug -> Aff (Either String Article)
getArticleBySlug storeRef slug = runExceptT do
  { slugToId, byId } <- liftAff $ Avar.take storeRef
  articleId <- maybeThrow ("Could not find article with slug " <> show slug) $ Map.lookup slug slugToId
  maybeThrow ("Could not find article with id " <> show articleId) $ Map.lookup articleId byId

listArticles :: AVar MemStore -> Aff (List Article)
listArticles storeRef = do
  { byId } <- liftAff $ Avar.take storeRef
  pure $ foldl (\acc article -> (acc <> List.singleton article)) (Nil :: List Article) $ Map.values byId

updateArticle :: AVar MemStore -> ArticleId -> (Article -> Article) -> Aff (Either String Article)
updateArticle storeRef articleId updateFn = runExceptT do
  { byId, slugToId } <- liftAff $ Avar.take storeRef
  article <- maybeThrow ("Could not find article with id " <> show articleId) $ Map.lookup articleId byId

  let
    updatedArticle = updateFn article
    newStore = { byId: Map.insert articleId updatedArticle byId, slugToId }

  liftAff $ Avar.put newStore storeRef
  pure updatedArticle

deleteArticle :: AVar MemStore -> ArticleId -> Aff (Either String Unit)
deleteArticle storeRef articleId = runExceptT do
  { byId, slugToId } <- liftAff $ Avar.take storeRef
  (Article { slug }) <- maybeThrow ("Could not find article with id " <> show articleId) $ Map.lookup articleId byId
  liftAff $ Avar.put { byId: Map.delete articleId byId, slugToId: Map.delete slug slugToId } storeRef

mkMemoryArticleStore :: ArticleMap -> Aff (ArticleRepo Aff)
mkMemoryArticleStore initialState = do
  let slugToId = foldl (\acc (Article { articleId, slug }) -> Map.insert slug articleId acc) Map.empty $ Map.values initialState
  storeRef <- Avar.new { byId: initialState, slugToId }
  pure $
    ArticleRepo
      { create: createArticle storeRef
      , getById: getArticleById storeRef
      , getBySlug: getArticleBySlug storeRef
      , list: listArticles storeRef
      , update: updateArticle storeRef
      , delete: deleteArticle storeRef
      }
