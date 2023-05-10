module Server.App.Driven.ArticleRepo.MemStore
  ( mkMemoryArticleStore
  ) where

import Prelude

import Conduit.Data.Limit (Limit(..))
import Conduit.Data.UserId (AuthorId)
import Data.Array (drop, elem, filter, singleton, take)
import Data.Foldable (foldl)
import Data.List (toUnfoldable)
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
import Server.Core.Ports.Ports (ArticleCreateInput, ArticleRepo(..), ArticleListInput)
import Slug (Slug, generate)
import Yoga.Om (Om, throw, note)

type ArticleMap = Map.Map ArticleId Article
type SlugToArticle = Map.Map Slug ArticleId
type FavoritedBy = Map.Map ArticleId (Array AuthorId)

type MemStore =
  { byId :: ArticleMap
  , slugToId :: SlugToArticle
  , favoritedBy :: FavoritedBy
  }

createArticle :: AVar MemStore -> ArticleCreateInput -> Om {} (articleRepoErr :: String) Article
createArticle storeRef { title, description, body, tagList, authorId } = do

  { byId, slugToId, favoritedBy } <- liftAff $ Avar.take storeRef

  slug <- case generate title of
    Just slug -> do
      if Map.member slug slugToId then
        throw { articleRepoErr: "Expected unique slug but found a duplicate for " <> (show slug) }
      else
        pure slug
    Nothing -> throw { articleRepoErr: "Expected a slug-able title but could not generate from " <> title }

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
      , tagList: fromMaybe [] tagList
      , createdAt: now
      , updatedAt: Nothing
      }
    newStore =
      { byId: Map.insert articleId article byId
      , slugToId: Map.insert slug articleId slugToId
      , favoritedBy: Map.insert articleId [] favoritedBy
      }

  liftAff $ Avar.put newStore storeRef
  pure article

getArticleById :: AVar MemStore -> ArticleId -> Om {} (articleRepoErr :: String) Article
getArticleById storeRef articleId = do
  { byId } <- liftAff $ Avar.take storeRef
  note { articleRepoErr: "Could not find article with id " <> show articleId } $ Map.lookup articleId byId

getArticleBySlug :: AVar MemStore -> Slug -> Om {} (articleRepoErr :: String) Article
getArticleBySlug storeRef slug = do
  { slugToId, byId } <- liftAff $ Avar.take storeRef
  articleId <- note { articleRepoErr: "Could not find article with slug " <> show slug } $ Map.lookup slug slugToId
  note { articleRepoErr: "Could not find article with id " <> show articleId } $ Map.lookup articleId byId

queryBuilder :: ArticleListInput -> ((Tuple Article (Array AuthorId)) -> Boolean)
queryBuilder { tag: maybeTag, author: maybeAuthor, favorited: maybeFavorited } =
  let
    tagFilter = case maybeTag of
      Just tag -> \article -> elem tag article.tagList
      Nothing -> const true
    authorFilter = case maybeAuthor of
      Just author -> \article -> article.authorId == author
      Nothing -> const true
    favoritedByFilter = case maybeFavorited of
      Just favorited -> \(_ /\ favoritedBy) -> elem favorited $ favoritedBy
      Nothing -> const true
  in
    \(article /\ favoritedBy) -> tagFilter article && authorFilter article && favoritedByFilter (article /\ favoritedBy)

listArticles :: AVar MemStore -> ArticleListInput -> Om {} (articleRepoErr :: String) (Array Article)
listArticles storeRef input@{ limit, offset } =
  do
    { byId, favoritedBy } <- liftAff $ Avar.take storeRef
    pure
      $ foldl
          ( \acc (article /\ _) ->
              (acc <> singleton article)
          )
          ([])
      $ filter (queryBuilder input)
      $ map (\article -> Tuple article $ fromMaybe [] $ Map.lookup article.articleId favoritedBy)
      $ take (unwrap (fromMaybe (Limit 20) limit))
      $ drop (fromMaybe 0 offset)
      $ toUnfoldable
      $ Map.values byId

updateArticle :: AVar MemStore -> ArticleId -> (Article -> Article) -> Om {} (articleRepoErr :: String) Article
updateArticle storeRef articleId updateFn = do
  store@{ byId } <- liftAff $ Avar.take storeRef
  article <- note { articleRepoErr: "Could not find article with id " <> show articleId } $ Map.lookup articleId byId

  let
    updatedArticle = updateFn article
    newStore = store { byId = Map.insert articleId updatedArticle byId }

  liftAff $ Avar.put newStore storeRef
  pure updatedArticle

deleteArticle :: AVar MemStore -> ArticleId -> Om {} (articleRepoErr :: String) Unit
deleteArticle storeRef articleId = do
  { byId, slugToId, favoritedBy } <- liftAff $ Avar.take storeRef
  { slug } <- note { articleRepoErr: "Could not find article with id " <> show articleId } $ Map.lookup articleId byId
  liftAff
    $ Avar.put
        { byId: Map.delete articleId byId
        , slugToId: Map.delete slug slugToId
        , favoritedBy: Map.delete articleId favoritedBy
        }
        storeRef

mkMemoryArticleStore :: ArticleMap -> Aff ArticleRepo
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
