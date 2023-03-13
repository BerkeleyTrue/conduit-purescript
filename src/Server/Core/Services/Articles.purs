module Server.Core.Services.Articles where

import Data.Either (Either)
import Server.Core.Domain.Article (Article, ArticleId)
import Server.Core.Ports.Ports (ArticleRepo(..))

newtype ArticleService m = ArticleService
  { getArticle :: ArticleId -> m (Either String Article)
  }

mkArticleService :: forall m. ArticleRepo m -> ArticleService m
mkArticleService (ArticleRepo { getById }) = ArticleService { getArticle: getById }
