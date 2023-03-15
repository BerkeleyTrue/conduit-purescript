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
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.UUID (genUUID)
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Effect.Aff.AVar as Avar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Now (nowDate)
import Server.Core.Domain.Article (Article, ArticleId(..))
import Server.Core.Domain.User (AuthorId)
import Server.Core.Ports.Ports (ArticleCreateInput, ArticleRepo(..), ArticleListInput)
import Server.Infra.Data.Route (Limit(..))
import Slug (Slug, generate)

type ArticleMap = Map.Map ArticleId Article
type SlugToArticle = Map.Map Slug ArticleId
type FavoritedBy = Map.Map ArticleId (List AuthorId)

type MemStore =
  { byId :: ArticleMap
  , slugToId :: SlugToArticle
  , favoritedBy :: FavoritedBy
  }

createArticle :: AVar MemStore -> ArticleCreateInput -> Aff (Either String Article)
createArticle storeRef { title, description, body, tagList, authorId } = runExceptT do

  { byId, slugToId, favoritedBy } <- liftAff $ Avar.take storeRef

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
    article =
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
      , favoritedBy: Map.insert articleId Nil favoritedBy
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

queryBuilder :: ArticleListInput -> ((Tuple Article (List AuthorId)) -> Boolean)
queryBuilder { tag: maybeTag, author: maybeAuthor, favorited: maybeFavorited } =
  let
    tagFilter = case maybeTag of
      Just tag -> \article -> List.elem tag article.tagList
      Nothing -> const true
    authorFilter = case maybeAuthor of
      Just author -> \article -> article.authorId == author
      Nothing -> const true
    favoritedByFilter = case maybeFavorited of
      Just favorited -> \(_ /\ favoritedBy) -> List.elem favorited $ favoritedBy
      Nothing -> const true
  in
    \(article /\ favoritedBy) -> tagFilter article && authorFilter article && favoritedByFilter (article /\ favoritedBy)

listArticles :: AVar MemStore -> ArticleListInput -> Aff (List Article)
listArticles storeRef input@{ limit, offset } =
  do
    { byId, favoritedBy } <- liftAff $ Avar.take storeRef
    pure
      $ foldl
          ( \acc (article /\ _) ->
              (acc <> List.singleton article)
          )
          (Nil :: List Article)
      $ List.filter (queryBuilder input)
      $ map (\article -> Tuple article $ fromMaybe Nil $ Map.lookup article.articleId favoritedBy)
      $ List.take (unwrap (fromMaybe (Limit 20) limit))
      $ List.drop (fromMaybe 0 offset)
      $ Map.values byId

updateArticle :: AVar MemStore -> ArticleId -> (Article -> Article) -> Aff (Either String Article)
updateArticle storeRef articleId updateFn = runExceptT do
  store@{ byId } <- liftAff $ Avar.take storeRef
  article <- maybeThrow ("Could not find article with id " <> show articleId) $ Map.lookup articleId byId

  let
    updatedArticle = updateFn article
    newStore = store { byId = Map.insert articleId updatedArticle byId }

  liftAff $ Avar.put newStore storeRef
  pure updatedArticle

deleteArticle :: AVar MemStore -> ArticleId -> Aff (Either String Unit)
deleteArticle storeRef articleId =
  runExceptT do
    { byId, slugToId, favoritedBy } <- liftAff $ Avar.take storeRef
    { slug } <- maybeThrow ("Could not find article with id " <> show articleId) $ Map.lookup articleId byId
    liftAff
      $ Avar.put
          { byId: Map.delete articleId byId
          , slugToId: Map.delete slug slugToId
          , favoritedBy: Map.delete articleId favoritedBy
          }
          storeRef

mkMemoryArticleStore :: ArticleMap -> Aff (ArticleRepo Aff)
mkMemoryArticleStore initialState = do
  let slugToId = foldl (\acc { articleId, slug } -> Map.insert slug articleId acc) Map.empty $ Map.values initialState
  storeRef <- Avar.new { byId: initialState, slugToId, favoritedBy: Map.empty }
  pure $
    ArticleRepo
      { create: createArticle storeRef
      , getById: getArticleById storeRef
      , getBySlug: getArticleBySlug storeRef
      , list: listArticles storeRef
      , update: updateArticle storeRef
      , delete: deleteArticle storeRef
      }
