module Server.App.Driven.ArticleRepo.MemStore
  ( mkMemoryArticleRepo
  ) where

import Prelude

import Conduit.Data.ArticleId (ArticleId, mkArticleId)
import Conduit.Data.MySlug (MySlug, generate)
import Conduit.Data.UserId (AuthorId)
import Data.Array (drop, elem, filter, singleton, take)
import Data.Foldable (foldl)
import Data.JSDate (now)
import Data.List (toUnfoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.AVar (AVar)
import Effect.Aff.AVar as Avar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Server.Core.Domain.Article (Article)
import Server.Core.Ports.Ports (ArticleCreateInput, ArticleRepo(..), ArticleListInput)
import Yoga.Om (Om, fromAff, note, throw)

type ArticleMap = Map.Map ArticleId Article
type SlugToArticle = Map.Map MySlug ArticleId
type FavoritedBy = Map.Map ArticleId (Array AuthorId)

type MemStore =
  { byId :: ArticleMap
  , slugToId :: SlugToArticle
  , favoritedBy :: FavoritedBy
  }

mkCreate :: AVar MemStore -> ArticleCreateInput -> Om {} (articleRepoErr :: String) Article
mkCreate storeRef { title, description, body, tagList, authorId } = do

  { byId, slugToId, favoritedBy } <- liftAff $ Avar.take storeRef

  slug <- case generate title of
    Just slug -> do
      if Map.member slug slugToId then
        throw { articleRepoErr: "Expected unique slug but found a duplicate for " <> (show slug) }
      else
        pure slug
    Nothing -> throw { articleRepoErr: "Expected a slug-able title but could not generate from " <> title }

  articleId <- liftEffect $ mkArticleId
  now <- liftEffect $ now

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

mkGetById :: AVar MemStore -> ArticleId -> Om {} (articleRepoErr :: String) Article
mkGetById storeRef articleId = do
  { byId } <- liftAff $ Avar.take storeRef
  note { articleRepoErr: "Could not find article with id " <> show articleId } $ Map.lookup articleId byId

mkGetBySlug :: AVar MemStore -> MySlug -> Om {} (articleRepoErr :: String) Article
mkGetBySlug storeRef slug = do
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

mkList :: AVar MemStore -> ArticleListInput -> Om {} (articleRepoErr :: String) (Array Article)
mkList storeRef input@{ limit, offset } =
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
      $ take (fromMaybe 20 $ limit <#> unwrap)
      $ drop (fromMaybe 0 $ offset <#> unwrap)
      $ toUnfoldable
      $ Map.values byId

mkUpdate :: AVar MemStore -> MySlug -> (Article -> Article) -> Om {} (articleRepoErr :: String) Article
mkUpdate storeRef slug updateFn = do
  store@{ byId, slugToId } <- liftAff $ Avar.take storeRef
  articleId <- note { articleRepoErr: "Could not find article with slug " <> show slug } $ Map.lookup slug slugToId
  article <- note { articleRepoErr: "Could not find article with id " <> show articleId } $ Map.lookup articleId byId

  let updatedArticle = updateFn article

  newSlug <- note { articleRepoErr: "Could not generate slug from title " <> updatedArticle.title } $ generate updatedArticle.title

  let
    withUpdatedSlug = (updatedArticle { slug = newSlug })
    newStore = store
      { byId = Map.insert articleId withUpdatedSlug byId
      , slugToId = Map.insert newSlug articleId slugToId
      }

  liftAff $ Avar.put newStore storeRef
  pure updatedArticle

mkDelete :: AVar MemStore -> MySlug -> Om {} (articleRepoErr :: String) Unit
mkDelete storeRef slug = do
  { byId, slugToId, favoritedBy } <- liftAff $ Avar.take storeRef
  articleId <- note { articleRepoErr: "Could not find article with slug " <> show slug } $ Map.lookup slug slugToId
  liftAff
    $ Avar.put
        { byId: Map.delete articleId byId
        , slugToId: Map.delete slug slugToId
        , favoritedBy: Map.delete articleId favoritedBy
        }
        storeRef

mkMemoryArticleRepo :: ArticleMap -> Om {} () ArticleRepo
mkMemoryArticleRepo initialState = do
  let slugToId = foldl (\acc { articleId, slug } -> Map.insert slug articleId acc) Map.empty $ Map.values initialState
  storeRef <- fromAff $ Avar.new { byId: initialState, slugToId, favoritedBy: Map.empty }
  pure $
    ArticleRepo
      { create: mkCreate storeRef
      , getById: mkGetById storeRef
      , getBySlug: mkGetBySlug storeRef
      , list: mkList storeRef
      , update: mkUpdate storeRef
      , delete: mkDelete storeRef
      }
