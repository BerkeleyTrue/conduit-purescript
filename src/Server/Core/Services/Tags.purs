module Server.Core.Services.Tags where

import Prelude

import Data.Array (concatMap, nub)
import Data.Maybe (Maybe(..))
import Justifill (justifill)
import Server.Core.Domain.Article (Tag)
import Server.Core.Services.Articles (ArticleService(..), ArticleServiceErrs)
import Yoga.Om (Om)

type TagServiceErrs r = (ArticleServiceErrs r)

newtype TagService = TagService
  { list :: Om {} (TagServiceErrs ()) (Array Tag)
  }

mkTagService :: ArticleService -> TagService
mkTagService (ArticleService { list }) = TagService
  { list: do
      tagss <- list { username: Nothing, input: justifill {} }
      pure $ nub $ concatMap (_.tagList) tagss
  }
