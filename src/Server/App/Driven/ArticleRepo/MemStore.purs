module Server.App.Driven.ArticleRepo.MemStore
  ( mkMemoryArticleRepo
  ) where

import Prelude

import Conduit.App.Winston (Logger, mkLogger, Level(..))
import Conduit.Data.ArticleId (ArticleId, mkArticleId)
import Conduit.Data.MySlug (MySlug, generate)
import Conduit.Data.UserId (UserId)
import Data.Array (drop, elem, filter, singleton, take)
import Data.Foldable (foldl)
import Data.JSDate (now)
import Data.List (toUnfoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Set as Set
import Effect.AVar (AVar)
import Effect.Aff.AVar as Avar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Server.Core.Domain.Article (Article)
import Server.Core.Ports.Ports (ArticleCreateInput, ArticleRepo(..), ArticleListInput)
import Yoga.Om (Om, fromAff, note, throw)

type ArticleMap = Map.Map ArticleId Article
type SlugToArticle = Map.Map MySlug ArticleId

type MemStore =
  { byId :: ArticleMap
  , slugToId :: SlugToArticle
  }

mkCreate :: AVar MemStore -> ArticleCreateInput -> Om {} (articleRepoErr :: String) Article
mkCreate storeRef { title, description, body, tagList, authorId } = do

  { byId, slugToId } <- liftAff $ Avar.take storeRef

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
      , tagList: fromMaybe [] tagList
      , createdAt: now
      , updatedAt: Nothing
      , favoritedBy: Set.empty
      }
    newStore =
      { byId: Map.insert articleId article byId
      , slugToId: Map.insert slug articleId slugToId
      }

  liftAff $ Avar.put newStore storeRef
  pure article

mkGetById :: AVar MemStore -> ArticleId -> Om {} (articleRepoErr :: String) Article
mkGetById storeRef articleId = do
  { byId } <- liftAff $ Avar.read storeRef
  note { articleRepoErr: "Could not find article with id " <> show articleId } $ Map.lookup articleId byId

mkGetBySlug :: AVar MemStore -> MySlug -> Om {} (articleRepoErr :: String) Article
mkGetBySlug storeRef slug = do
  { slugToId, byId } <- liftAff $ Avar.read storeRef
  articleId <- note { articleRepoErr: "Could not find article with slug " <> show slug } $ Map.lookup slug slugToId
  note { articleRepoErr: "Could not find article with id " <> show articleId } $ Map.lookup articleId byId

queryBuilder :: ArticleListInput -> Article -> Boolean
queryBuilder { tag: maybeTag, author: maybeAuthor, favorited: maybeFavorited } =
  let
    tagFilter = case maybeTag of
      Just tag -> \article -> elem tag article.tagList
      Nothing -> const true
    authorFilter = case maybeAuthor of
      Just author -> \article -> article.authorId == author
      Nothing -> const true
    favoritedByFilter = case maybeFavorited of
      Just (favorited :: UserId) -> \(article :: Article) -> Set.member favorited article.favoritedBy
      Nothing -> const true
  in
    \article -> tagFilter article && authorFilter article && favoritedByFilter article

mkList :: Logger -> AVar MemStore -> ArticleListInput -> Om {} (articleRepoErr :: String) (Array Article)
mkList logger storeRef input@{ limit, offset } = do
  liftEffect $ logger.debug $ "ArticleRepo.mkList: " <> show input
  { byId } <- liftAff $ Avar.read storeRef
  liftEffect $ logger.debug $ "ArticleRepo.mkList: byId: " <> show byId
  pure
    $ foldl (\acc article -> (acc <> singleton article)) ([])
    $ filter (queryBuilder input)
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
  { byId, slugToId } <- liftAff $ Avar.take storeRef
  articleId <- note { articleRepoErr: "Could not find article with slug " <> show slug } $ Map.lookup slug slugToId
  liftAff
    $ Avar.put
        { byId: Map.delete articleId byId
        , slugToId: Map.delete slug slugToId
        }
        storeRef

mkMemoryArticleRepo :: ArticleMap -> Om {} () ArticleRepo
mkMemoryArticleRepo initialState = do
  logger <- liftEffect $ mkLogger Warning "app:driven:articlerepo"
  let slugToId = foldl (\acc { articleId, slug } -> Map.insert slug articleId acc) Map.empty $ Map.values initialState
  storeRef <- fromAff $ Avar.new { byId: initialState, slugToId }
  pure $
    ArticleRepo
      { create: mkCreate storeRef
      , getById: mkGetById storeRef
      , getBySlug: mkGetBySlug storeRef
      , list: mkList logger storeRef
      , update: mkUpdate storeRef
      , delete: mkDelete storeRef
      }
