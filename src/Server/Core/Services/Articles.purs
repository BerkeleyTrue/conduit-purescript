module Server.Core.Services.Articles where

import Prelude

import Data.Maybe (Maybe)
import Server.Core.Domain.Article (Article, ArticleId)
import Server.Core.Ports.Ports (ArticleRepo(..))

newtype ArticleService m = ArticleService
  { getArticle :: ArticleId -> m (Maybe Article)
  }

mkArticleService :: forall m. ArticleRepo m -> ArticleService m
mkArticleService (ArticleRepo { getById }) = ArticleService { getArticle: getById }
