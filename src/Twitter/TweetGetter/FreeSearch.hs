{-# LANGUAGE UndecidableInstances #-}

module Twitter.TweetGetter.FreeSearch
  ( FreeSearch(..)
  , FreeSearchT(..)

  , MockedCallT(..)
  , mockTweets
  )
  where

import Twitter.Auth
import Twitter.Call
import Twitter.TweetGetter.MonadRapperTweetsGetter
import Twitter.TweetGetter.SearchResult
import Twitter.User

import Protolude

import qualified Data.Time                            as T
import qualified Web.Twitter.Conduit                  as Twitter
import qualified Web.Twitter.Conduit.Request.Internal as Twitter
import qualified Web.Twitter.Conduit.Status           as Twitter
import qualified Web.Twitter.Types                    as Twitter

data FreeSearch
  = FreeSearch
      { maybeMaxId :: !(Maybe Twitter.StatusId)
      }

newtype FreeSearchT m a
  = FreeSearchT (m a)
  deriving newtype (Functor, Applicative, Monad, MonadCall)

deriving newtype instance MonadReader ctx m => MonadReader ctx (FreeSearchT m)

instance
  ( MonadCall m
  , HasTwitterAuth ctx m
  ) => MonadRapperTweetsGetter FreeSearch (FreeSearchT m)
  where
    getRapperTweets = getFreeSearchTweets

getFreeSearchTweets
  :: forall m context
   . ( HasTwitterAuth context m
     , MonadCall m
     )
  => FreeSearch
  -> m (RequestResult m)

getFreeSearchTweets FreeSearch{..} = do
  statuses <- call query

  let nextRequest
        = fmap (getFreeSearchTweets @m . FreeSearch . Just)
        $ headMay $ sort $ id <$> tweets

      tweets = statuses <&> \Twitter.Status{..} ->
        Tweet
          { id        = statusId
          , truncated = statusTruncated
          , text      = statusText
          , user      = statusUser
          }

  pure RequestResult{..}

  where
    query = freeSearchQuery { Twitter._params = params }

    Twitter.APIRequest{..} = freeSearchQuery
    params = case maybeMaxId of
      Nothing    -> _params
      Just maxId -> ("max_id", Twitter.PVInteger maxId) : _params

freeSearchQuery :: Twitter.APIRequest Twitter.StatusesUserTimeline [Twitter.Status]
freeSearchQuery = query
  { Twitter._params = ("count", Twitter.PVInteger 200) : Twitter._params query }
  where
    query = Twitter.userTimeline sendBeatsBotHandle


instance Monad m => MonadRapperTweetsGetter FreeSearch (MockedCallT m) where
  getRapperTweets searchRequest
    = pure RequestResult
        { tweets      = mockTweets
        , nextRequest = Just $ getRapperTweets searchRequest
        }

newtype MockedCallT m a
  = MockedCallT { runMockedCallT :: m a }
  deriving newtype (Functor, Applicative, Monad)

mockTweets :: [Tweet]
mockTweets =
  [ Tweet
      { id        = 1234132
      , truncated = False
      , text      = "Send me beats at test@email.com"
      , user      = mockUser
      }
  , Tweet
      { id        = 983741923841
      , truncated = False
      , text      = "Don't send me beats"
      , user      = mockUser
      }
  , Tweet
      { id        = 123412
      , truncated = True
      , text      = "Send some shit to test@gmail.com please boy"
      , user      = mockUser
      }
  ]

mockUser :: Twitter.User
mockUser = Twitter.User
  { userContributorsEnabled            = False
  , userCreatedAt                      = T.UTCTime (T.fromGregorian 2020 1 3) 0
  , userDefaultProfile                 = True
  , userDefaultProfileImage            = True
  , userDescription                    = Nothing
  , userEmail                          = Nothing
  , userFavoritesCount                 = 358
  , userFollowRequestSent              = Nothing
  , userFollowing                      = Nothing
  , userFollowersCount                 = 413
  , userFriendsCount                   = 123
  , userGeoEnabled                     = False
  , userId                             = 11111
  , userIsTranslator                   = False
  , userLang                           = Nothing
  , userListedCount                    = 1234
  , userLocation                       = Nothing
  , userName                           = "supertestuser"
  , userNotifications                  = Nothing
  , userProfileBackgroundColor         = Nothing
  , userProfileBackgroundImageURL      = Nothing
  , userProfileBackgroundImageURLHttps = Nothing
  , userProfileBackgroundTile          = Nothing
  , userProfileBannerURL               = Nothing
  , userProfileImageURL                = Nothing
  , userProfileImageURLHttps           = Nothing
  , userProfileLinkColor               = "idk what's supposed to be here"
  , userProfileSidebarBorderColor      = "maybe lol"
  , userProfileSidebarFillColor        = "lalalaa"
  , userProfileTextColor               = "idk"
  , userProfileUseBackgroundImage      = False
  , userProtected                      = False
  , userScreenName                     = "Cool Test Guy"
  , userShowAllInlineMedia             = Nothing
  , userStatusesCount                  = 343
  , userTimeZone                       = Nothing
  , userURL                            = Nothing
  , userUtcOffset                      = Nothing
  , userVerified                       = True
  , userWithheldInCountries            = Nothing
  , userWithheldScope                  = Nothing
  }
