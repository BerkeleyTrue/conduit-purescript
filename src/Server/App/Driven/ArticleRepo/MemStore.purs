module Server.App.Driven.ArticleRepo.MemStore where

import Prelude

import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.List.Trans (ListT, foldl)
import Data.Array (fromFoldable)
import Data.Either (Either(..), note)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.UUID (genUUID)
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Effect.Aff.AVar as AVar
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

  store@{ byId, slugToId } <- liftAff $ Avar.take storeRef

  (slug :: Slug) <- case generate title of
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
      , tagList: fromMaybe [] tagList
      , createdAt: now
      , updatedAt: Nothing
      }
    newStore = { byId: Map.insert articleId article byId, slugToId: Map.insert slug articleId slugToId }

  liftAff $ Avar.put newStore storeRef
  pure article

mkMemoryArticleStore :: ArticleMap -> Aff (ArticleRepo Aff)
mkMemoryArticleStore initialState = do
  slugToId <- foldl (\acc (articleId /\ (Article { slug })) -> Map.insert slug articleId acc) Map.empty $ Map.toUnfoldable initialState
  storeRef <- AVar.new { byId: initialState, slugToId }
  pure $
    ArticleRepo
      { create: createArticle storeRef
      , getById: ?_
      , getBySlug: ?_
      , list: ?_
      , update: ?_
      , delete: ?_
      }
