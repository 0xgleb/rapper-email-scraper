{-# OPTIONS_GHC -Wno-orphans #-}

module Scraper.EmailSpec (spec) where

import FileManager
import Scraper.Email
import Twitter.TweetGetter.FreeSearch
import Util


import Protolude
import Test.Hspec

import qualified Data.Map.Strict as Map


testTweetList :: [(Email, Text)]
testTweetList =
  [ ( Email "realskinny24@gmail.com"
    , "RT @FrSkinny: I’m tryna spill my feelings on a track send me 💔 beats. Realskinny24@gmail.com"
    )

  , ( Email "juicesinatra23@gmail.com"
    , "RT @juicesinatra: Producers send fire 🔥 beats please juicesinatra23@gmail.com"
    )

  , ( Email "2jtherichest@gmail.com"
    , "RT @2jtherichest: We goin 6/6 in the stu Tn 🤍 send ya beats 2jtherichest@gmail.com"
    )

  , ( Email "cashboyprod@gmail.com"
    , "RT @MaccoTheprince: cooked soo many beats today. Asked yall to send in loops\n\n\
      \cashboyprod@gmail.com"
    )

  , ( Email "kicpimpcartel1@gmail.com"
    , "RT @KicPimp: Send me hard beats rn now @kicpimpcartel1@gmail.com 🔥🔥🔥🔥"
    )

  , ( Email "crossorjiako1@gmail.com"
    , "RT @krosszn: producers on here send me beats !let’s make magic !❤️ crossorjiako1@gmail.com"
    )

  , ( Email "beatspramim@gmail.com"
    , "RT @LilMezu: @Nerpixn send me beats for payment  &gt; beatspramim@gmail.com"
    )

  , ( Email "butterzzbeats10@hotmail.com"
    , "RT @butterzzg: Send beats to \nButterzzbeats10@hotmail.com"
    )

  , ( Email "djconz@hotmail.com"
    , "RT @DJ_CONZ: SEND BEATS TO ....\n\nDjconz@hotmail.com \n\n🌐 📥"
    )

  , ( Email "menofrespectatl@gmail.com"
    , "RT @Mase_Milli: Send hot 🔥🔥🔥🔥 Trap, Pop, R&amp;B, &amp; Up-Tempo Beats to:\
      \ Menofrespectatl@gmail.com Bangin Beats Only!!!!!"
    )

  , ( Email "selfmademizzybeats@gmail.com"
    , "RT @SelfMadeMizzy: Send beats to selfmademizzybeats@gmail.com"
    )

  , ( Email "jackcorpirations@gmail.com"
    , "RT @DrelilJack: Send all #beats to jackcorpirations@gmail.com all that sees\
      \ #producers #nyc #music #writer #rap #hiphop\n#imback thank you a…"
    )

  , ( Email "beats4mickey@gmail.com"
    , "RT @MickeyMaroa: Send beats to Beats4Mickey@gmail.com"
    )

  , ( Email "swaggertown.mgmt@gmail.com"
    , "RT @JakeStrain: I'm willing to work with upcoming producers send beats to my\
      \ management Swaggertown.MGMT@gmail.com\n\nOnly fire beats 🔥 no tr…"
    )

  , ( Email "toxepicxtoxfail@gmail.com"
    , "RT @KilosSinatra: I’m locked in I need smooth melodic beats\
      \ send em to me toxepicxtoxfail@gmail.com"
    )

  , ( Email "contact@lllmanagement.com"
    , "RT @AtmCurly: Send beats to contact@lllmanagement.com"
    )
  ]

spec :: Spec
spec = do
  describe "findEmailInText" $ do
    void $ forM testTweetList $ \((email, tweet)) ->
      it ("finds email " <> show email <> " in the corresponding tweet") $
        findEmailInText tweet `shouldBe` Just email

  describe "extractEmailsFromTweets" $ do
    it "can extract and save emails from the tweets you give it" $ do
      let extractionResult
            = fst $ flip runState (Map.fromList []) $ runStateFileManager
            $ fmap fst $ flip runStateT [] $ runMockSayT
            $ flip runReaderT extractTestContext $ runTestExtractEmailsFromTweets $ do
                let filePath = "rapper-emails.txt"
                initEmails <- readEmails @_ @Email filePath
                let initiallyEmpty = initEmails == []
                extractEmailsFromTweets @ExtractTestContext @ExtractTestContext []
                noChangeEmails <- readEmails @_ @Email filePath
                let extractingEmptyListMakesNoChange = noChangeEmails == []
                extractEmailsFromTweets @ExtractTestContext @ExtractTestContext mockTweets
                emails <- readEmails @_ @Email filePath
                let extractingWorks = expectedEmails == emails
                extractEmailsFromTweets @ExtractTestContext @ExtractTestContext mockTweets
                finalEmails <- readEmails @_ @Email filePath
                let noDuplicates = expectedEmails == finalEmails
                pure ExtractionResult{..}
      finalEmails extractionResult `shouldBe` expectedEmails
      extractionResult `shouldBe` positiveExtractionResult

expectedEmails :: [Email]
expectedEmails
  = Email <$> ["test@email.com", "test@gmail.com"]

data ExtractionResult
  = ExtractionResult
      { initiallyEmpty                   :: Bool
      , extractingEmptyListMakesNoChange :: Bool
      , extractingWorks                  :: Bool
      , noDuplicates                     :: Bool
      , finalEmails                        :: [Email]
      }
  deriving stock (Eq, Show)

positiveExtractionResult :: ExtractionResult
positiveExtractionResult =  ExtractionResult True True True True expectedEmails

newtype TestExtractEmailsFromTweets a
  = TestExtractEmailsFromTweets
      { runTestExtractEmailsFromTweets
          :: ReaderT ExtractTestContext (MockSayT StateFileManager) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader ExtractTestContext)

data ExtractTestContext
  = ExtractTestContext
      { userId :: Integer
      }
  deriving stock (Generic)

extractTestContext :: ExtractTestContext
extractTestContext
  = ExtractTestContext
      { userId = 11111
      }

instance MonadFileManager TestExtractEmailsFromTweets where
  doesFileExist = TestExtractEmailsFromTweets . lift . doesFileExist
  readJSONFile  = TestExtractEmailsFromTweets . lift . readJSONFile
  readEmails    = TestExtractEmailsFromTweets . lift . readEmails

  writeJSONFile = (TestExtractEmailsFromTweets . lift) ... writeJSONFile

  saveUnsavedEmails filePath
    = (TestExtractEmailsFromTweets . lift) ... saveUnsavedEmails filePath

instance MonadFileManager (MockSayT StateFileManager) where
  doesFileExist = MockSayT . lift . doesFileExist
  readJSONFile  = MockSayT . lift . readJSONFile
  readEmails    = MockSayT . lift . readEmails

  writeJSONFile = (MockSayT . lift) ... writeJSONFile

  saveUnsavedEmails filePath
    = (MockSayT . lift) ... saveUnsavedEmails filePath

instance MonadSay TestExtractEmailsFromTweets where
  say = TestExtractEmailsFromTweets . lift . say
