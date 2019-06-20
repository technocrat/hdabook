{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

{-|

Module      : LearningDataAnalysis07
Description : Contains functions for the classification of sentences into
              the language used based on tweets downloaded from the Twitter
              API.
Maintainer  : jcchurch@gmail.com

To install OAuth, Aeson, Network HTTP, and HTTP Conduit, use cabal.

cabal install authenticate-oauth aeson-utils http http-conduit

This code requires that you modify the code with your Twitter API
Credentials and will not work unless the instructions described
in the chapter are followed.

All GHCi examples used in this chapter:

:l LearningDataAnalysis02 LearningDataAnalysis04 LearningDataAnalysis07
:m LearningDataAnalysis02 LearningDataAnalysis04 LearningDataAnalysis07
import Data.HashMap.Strict as HM
import Data.List as L
createTweetsDatabase
mapM_ (\x -> collectTweetsIntoDatabase) [1..180]
sqlTweets <- queryDatabase "tweets.sql" "SELECT message, language FROM tweets"
let tweets = zip (readStringColumn sqlTweets 0) (readStringColumn sqlTweets 1)
let freqTable = frequency tweets
let uniqueTweets = HM.keys freqTable
HM.size freqTable
let cleanedTweets = zip (L.map (removePunctuation.clean.fst) uniqueTweets) (L.map snd uniqueTweets)
let languageFrequency = (frequency . L.map snd) cleanedTweets
languageFrequency
let allLanguages = HM.keys languageFrequency
length allLanguages
let wordFrequency = (frequency . concatMap words) (L.map fst cleanedTweets)
size wordFrequency
let wordFrequencyByLanguage = (HM.fromList . L.map (\language -> (language, (frequency . concatMap words . L.map fst) (L.filter (\tweet -> language == (snd tweet)) cleanedTweets)))) allLanguages
probLanguageGivenWord "en" "house" languageFrequency wordFrequency wordFrequencyByLanguage
probLanguageGivenWord "es" "house" languageFrequency wordFrequency wordFrequencyByLanguage
probLanguageGivenWord "pt" "house" languageFrequency wordFrequency wordFrequencyByLanguage
probLanguageGivenWord "fr" "house" languageFrequency wordFrequency wordFrequencyByLanguage
probLanguageGivenWord "en" "casa" languageFrequency wordFrequency wordFrequencyByLanguage
probLanguageGivenWord "es" "casa" languageFrequency wordFrequency wordFrequencyByLanguage
probLanguageGivenWord "pt" "casa" languageFrequency wordFrequency wordFrequencyByLanguage
probLanguageGivenWord "fr" "casa" languageFrequency wordFrequency wordFrequencyByLanguage
probLanguageGivenMessage "en" "my house is your house" languageFrequency wordFrequencyByLanguage
probLanguageGivenMessage "es" "my house is your house" languageFrequency wordFrequencyByLanguage
probLanguageGivenMessage "en" "mi casa su casa" languageFrequency wordFrequencyByLanguage
probLanguageGivenMessage "es" "mi casa su casa" languageFrequency wordFrequencyByLanguage
maxClassifier $ languageClassifierGivenMessage "the quick brown fox jumps over the lazy dog" languageFrequency wordFrequencyByLanguage
maxClassifier $ languageClassifierGivenMessage "hi how are you doing today" languageFrequency wordFrequencyByLanguage
maxClassifier $ languageClassifierGivenMessage "it is a beautiful day outside" languageFrequency wordFrequencyByLanguage
maxClassifier $ languageClassifierGivenMessage "would you like to join me for lunch" languageFrequency wordFrequencyByLanguage
maxClassifier $ languageClassifierGivenMessage "my teacher gave me too much homework" languageFrequency wordFrequencyByLanguage
maxClassifier $ languageClassifierGivenMessage "estoy bien gracias" languageFrequency wordFrequencyByLanguage
maxClassifier $ languageClassifierGivenMessage "vaya ud derecho" languageFrequency wordFrequencyByLanguage
maxClassifier $ languageClassifierGivenMessage "eres muy amable" languageFrequency wordFrequencyByLanguage
maxClassifier $ languageClassifierGivenMessage "le gusta a usted aquí" languageFrequency wordFrequencyByLanguage
maxClassifier $ languageClassifierGivenMessage "feliz cumpleaños" languageFrequency wordFrequencyByLanguage
maxClassifier $ languageClassifierGivenMessage "cest une bonne idée" languageFrequency wordFrequencyByLanguage
maxClassifier $ languageClassifierGivenMessage "il est très beau" languageFrequency wordFrequencyByLanguage

-}

module LearningDataAnalysis07 where

import Data.Ord
import Data.List as L
import Data.Hashable
import Data.HashMap.Strict as HM
import Database.HDBC.Sqlite3
import Database.HDBC
import Control.Concurrent
import Data.Char

import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Data.Aeson
import GHC.Generics

-- Insert here your own credentials

myoauth :: OAuth
myoauth =
  newOAuth { oauthServerName     = "api.twitter.com"
           , oauthConsumerKey    = "YOUR CONSUMER KEY"
           , oauthConsumerSecret = "YOUR CONSUMER SECRET KEY"
             }

mycred :: Credential
mycred = newCredential "YOUR ACCESS TOKEN"
                       "YOUR ACCESS TOKEN SECRET"

data User =
    User { screenName :: !String } deriving (Show, Generic)

data Status =
  Status { text :: !String,
          lang :: !String,
          user :: !User } deriving (Show, Generic)

data Search =
  Search { statuses :: ![Status] } deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

instance FromJSON Status
instance ToJSON Status

instance FromJSON Search
instance ToJSON Search

{-|
   Creates a new database for the purpose of storing a tweet,
   the member who wrote the tweet, and the language determined
   by Twitter.
   Prints "Successfully created database." if successful,
   or prints error message otherwise.
-}
createTweetsDatabase :: IO()
createTweetsDatabase = do
      conn <- connectSqlite3 "tweets.sql"
      _ <- run conn createStatement []
      commit conn
      disconnect conn
      putStrLn "Successfully created database."
  where
      createStatement =
        "CREATE TABLE tweets (message TEXT, user TEXT, language TEXT)"

{-|
   Given a list of twitter Statuses, inserts each into
   the tweets database. This assumes a call to 
   createTweetsDatabase has been previously called.
   Upon a successful insertion, the message
   "Successfully inserted Tweets to database." will be printed
   to the screen.
-}
insertTweetsInDatabase :: [Status] -> IO()
insertTweetsInDatabase tweets = do
      conn <- connectSqlite3 "tweets.sql"
      stmt <- prepare conn insertStatement
      executeMany stmt sqlRecords
      commit conn
      disconnect conn
      putStrLn "Successfully inserted Tweets to database."

  where
      insertStatement = "INSERT INTO tweets VALUES (?, ?, ?)"
      sqlRecords = L.map
                   (\(Status aMessage aLanguage (User aUser))
                      -> [toSql aMessage, toSql aUser, toSql aLanguage])
                   tweets

{-|
   Given a search term, makes a call to the Twitter Search
   API and parses the JSON response into a Search object.
   Returns either a Search object or a String error message.
-}
twitterSearch :: String -> IO (Either String Search)
twitterSearch term = do
      req <- parseUrl $ "https://api.twitter.com/1.1/search/tweets.json?count=200&q=" ++ term
      res <- withManager $ \m -> do
              signedreq <- signOAuth myoauth mycred req
              httpLbs signedreq m
      return $ eitherDecode $ responseBody res

{-|
    Prints a Search object to the screen.
-}
printTweets :: Search -> IO ()
printTweets = mapM_ print . statuses

{-|
    Calls the twitterSearch function using the search term "a"
    and inserts the responses into the database by calling
    insertTweetsInDatabase. At the end of each call, a 5 second
    delay is imposed.
-}
collectTweetsIntoDatabase :: IO()
collectTweetsIntoDatabase = do
      aSearch <- twitterSearch "a"
      either
         putStrLn
         (insertTweetsInDatabase . statuses)        
         aSearch
      threadDelay 5000

{-|
    Given a list of Hashable values, returns a HashMap where each
    value is represented by a key and that key's associated value
    is the number of times the key appears in the list.
-}
frequency :: (Eq k, Data.Hashable.Hashable k, Integral v) => [k] -> HashMap k v
frequency [] = empty
frequency (x:xs) = insertWith (+) x 1 (frequency xs)

{-|
    Given a string, all characters which are non-alphanumeric and non-space
    are filtered out and returned.
-}
removePunctuation :: String -> String
removePunctuation myString =
      [toLower c | c <- myString, or [isAlpha c, isSpace c]]

{-|
    Given a tweet message, this function filters out hashtags, links, and
    referenced members from the tweet and returns it.
-}
clean :: String -> String
clean myString = unwords $ L.filter
      (\myWord -> not (or
                  [ "@" `isInfixOf` myWord
                  , "#" `isInfixOf` myWord
                  , "http://" `isInfixOf` myWord ]))
      (words myString)

{-|
   Given a langauge, a word (known), a language frequency HashMap,
   a word frequency HashMap, and a word frequency by language HashMap, 
   this function returns the Naive Bayes probability of the langauge
   given this word.
-}
probLanguageGivenWord ::
    String
    -> String
    -> HashMap String Integer
    -> HashMap String Integer
    -> HashMap String (HashMap String Integer)
    -> Double
probLanguageGivenWord
    language
    word
    languageFrequency
    wordFrequency
    wordFrequencyByLanguage =
      pLanguage * pWordGivenLanguage / pWord
  where
      countTweets = fromIntegral . sum $ elems languageFrequency
      countAllWords = fromIntegral . sum $ elems wordFrequency
      countLanguage = fromIntegral $ lookupDefault 0 language languageFrequency
      countWordsUsedInLanguage =
          fromIntegral . sum . elems $ wordFrequencyByLanguage ! language
      countWord = fromIntegral $ lookupDefault 0 word wordFrequency
      countWordInLanguage =
          fromIntegral $ lookupDefault 0 word (wordFrequencyByLanguage ! language)
      pLanguage = countLanguage / fromIntegral countTweets
      pWordGivenLanguage = countWordInLanguage / countWordsUsedInLanguage
      pWord = countWord / countAllWords

{-|
    Given a word, a language (known), and a word frequency by language
    HashMap, returns the probability of this word representing this
    language.
-}
probWordGivenLanguage ::
    String
    -> String
    -> HashMap String (HashMap String Integer)
    -> Double
probWordGivenLanguage
    word
    language
    wordFrequencyByLanguage =
      countWordInLanguage / countWordsUsedInLanguage
  where
      countWordInLanguage =
          fromIntegral . lookupDefault 0 word $ wordFrequencyByLanguage ! language
      countWordsUsedInLanguage = 
          fromIntegral . sum . elems $ wordFrequencyByLanguage ! language

{-|
    Given a language and a language frequency HashMap, returns
    the prior probability of this language.
-}
probLanguage :: String -> HashMap String Integer -> Double
probLanguage language languageFrequency =
      countLanguage / countTweets
  where
      countTweets = fromIntegral . sum $ elems languageFrequency
      countLanguage = fromIntegral $ lookupDefault 0 language languageFrequency

{-|
    Given a langauge, a message (known), a language frequency HashMap, and
    a word frequency by langauge HashMap, returns a Naive Bayes score
    of this language representing this message. Note: this function does
    not return a true probability.
-}
probLanguageGivenMessage ::
    String
    -> String
    -> HashMap String Integer
    -> HashMap String (HashMap String Integer)
    -> Double
probLanguageGivenMessage
    language
    message
    languageFrequency
    wordFrequencyByLanguage =
      probLanguage language languageFrequency
      * product (L.map
                 (\word ->
                     probWordGivenLanguage word language wordFrequencyByLanguage)
                (words message))

{-|
    Given a message, a language frequency hashMap and a word frequency
    by language HashMap, returns a list of tuples consisting of each
    language represented and the associated Naive Bayes score for this
    message. The language with the highest score should be the language
    representing this message.
-}
languageClassifierGivenMessage ::
    String
    -> HashMap String Integer
    -> HashMap String (HashMap String Integer)
    -> [(String, Double)]
languageClassifierGivenMessage
    message
    languageFrequency
    wordFrequencyByLanguage =
      L.map (\language ->
              (language,
                  probLanguageGivenMessage
                      language
                      message
                      languageFrequency
                      wordFrequencyByLanguage))
            (keys languageFrequency)

{-|
    Given a list of tuples consisting of (String, Double), returns
    which tuple has the largest Double value. We use this function
    in order to find the language with this highest Naive Bayes
    score.
-}
maxClassifier :: [(String, Double)] -> (String, Double)
maxClassifier = L.maximumBy (comparing snd)
