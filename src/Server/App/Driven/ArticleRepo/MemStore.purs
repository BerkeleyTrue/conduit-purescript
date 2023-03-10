module Server.App.Driven.ArticleRepo.MemStore where

import Prelude

import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.List.Trans (foldl)
import Data.Array (fromFoldable)
import Data.Either (Either(..), note)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Data.UUID (genUUID)
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Effect.Aff.AVar as AVar
import Effect.Aff.AVar as Avar
import Effect.Class (liftEffect)
import Effect.Now (nowDate)
import Server.Core.Domain.Article (Article(..), ArticleId(..))
import Server.Core.Ports.Ports (ArticleRepo, ArticleCreateInput)
import Slug (Slug, generate)

type ArticleMap = Map.Map ArticleId Article
type SlugToArticle = Map.Map Slug ArticleId

type MemStore =
  { byId :: ArticleMap
  , slugToId :: SlugToArticle
  }

createArticle :: AVar MemStore -> ArticleCreateInput -> Aff (Either String Article)
createArticle storeRef { title, description, body, tagList, authorId } = runExceptT do
  (slug :: Slug) <- note "Could not generate slug" $ generate title

  (store@{ byId, slugToId } :: MemStore) <- Avar.take storeRef

  when (Map.member slug slugToId) do
    throwError "Slug already exists"

  articleId <- liftEffect $ ArticleId <$> genUUID
  now <- liftEffect nowDate

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

  Avar.put newStore storeRef

  (pure $ Right article :: Aff (Either String Article))

mkMemoryArticleStore :: ArticleMap -> Aff (ArticleRepo Aff)
mkMemoryArticleStore initialState = do
  (slugToId :: SlugToArticle) <- foldl (\acc (articleId /\ { slug }) -> Map.insert slug articleId acc) Map.empty $ Map.toUnfoldable initialState
  (storeRef :: MemStore) <- AVar.new { byId: initialState, slugToId }
  pure
    { create: createArticle storeRef
    }
