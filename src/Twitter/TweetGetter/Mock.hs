module Twitter.TweetGetter.Mock
  ( MockedCallT(..)
  , mockEmails
  , mockTweets
  , mockUser
  )
  where

import Twitter.TweetGetter.SearchResult
import Scraper.Email

import Protolude

import qualified Data.Time         as T
import qualified Web.Twitter.Types as Twitter

newtype MockedCallT m a
  = MockedCallT { runMockedCallT :: m a }
  deriving newtype (Functor, Applicative, Monad)

mockEmails :: [Email]
mockEmails
  = Email <$> ["test@email.com", "test@gmail.com"]

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
