{-|
Module      : LearningDataAnalysis08
Description : Contains functions for performing Principal Component Analyis.
Maintainer  : jcchurch@gmail.com

You are going to need to install “lapack” on your system. “lapack” is short for Linear Algebra Package and its the collection of algorithms for solving common matrix related tasks, such as the eigenvalue decomposition problem that we need. On Debian based systems, you should be able to download it using apt-get.

sudo apt-get install liblapack-dev

You will also need to install the Haskell Matrix wrapper modules for “lapack” called “hmatrix” using cabal.

cabal install hmatrix

Windows users will have to abandon this chapter or attempt to install lapack on your own.

All GHCi examples used in this chapter:

:l LearningDataAnalysis04 LearningDataAnalysis06 LearningDataAnalysis07 LearningDataAnalysis08
:m LearningDataAnalysis04 LearningDataAnalysis06 LearningDataAnalysis07 LearningDataAnalysis08
import Data.HashMap.Strict as HM
import Data.List as L
tweetsEnglish <- queryDatabase "tweets.sql" "SELECT message, user FROM tweets WHERE language='en'"
let tweets = zip (readStringColumn tweetsEnglish 0) (readStringColumn tweetsEnglish 1)
let freqTable = frequency tweets
HM.size freqTable
let uniqueTweets = keys freqTable
let cleanedTweets = L.map (\(message, user) -> (removePunctuation $ clean message, user)) uniqueTweets
let wordsFrequency = frequency $ concatMap (words . fst) cleanedTweets
let topWords = take 25 $ sortBy (\(_, c1) (_, c2) -> compare c2 c1) $ HM.toList wordsFrequency
topWords
let notStopWords = L.filter (\(word, _) -> notElem word stopWords) (HM.toList wordsFrequency)
let topWords = take 25 . L.map fst $ sortBy (\(_, c1) (_, c2) -> compare c2 c1) notStopWords
topWords
let userFrequency = frequency $ L.map snd uniqueTweets
let allUsers = keys userFrequency
let wordFrequencyByUser = HM.fromList $ L.map (\user -> (user, frequency $ concatMap words [ message | (message, thisUser) <- cleanedTweets, thisUser == user ])) allUsers
let getFrequencyOfWordByUser user word = fromIntegral $ (HM.lookupDefault 0 word (wordFrequencyByUser HM.! user)) :: Double
let wordsMatrix = L.map (\user -> L.map (\word -> getFrequencyOfWordByUser user word) topWords) allUsers
import Numeric.LinearAlgebra.HMatrix
queryDatabase "winloss.sql" "SELECT COUNT(*) FROM winloss"
queryDatabase "winloss.sql" "SELECT COUNT(*) FROM winloss WHERE awayscore == homescore;"
homeRecord <- queryDatabase "winloss.sql" "SELECT homeTeam, SUM(homescore > awayscore), SUM(homescore), COUNT(*) FROM winloss GROUP BY homeTeam;"
awayRecord <- queryDatabase "winloss.sql" "SELECT awayTeam, SUM(awayscore > homescore), SUM(awayscore), COUNT(*) FROM winloss GROUP BY awayTeam;"
let totalWins = zipWith (+) (readDoubleColumn homeRecord 1) (readDoubleColumn awayRecord 1)
let totalRuns = zipWith (+) (readDoubleColumn homeRecord 2) (readDoubleColumn awayRecord 2)
let totalGames = zipWith (+) (readDoubleColumn homeRecord 3) (readDoubleColumn awayRecord 3)
let winPercentage = zipWith (/) totalWins totalGames
let runsPerGame = zipWith (/) totalRuns totalGames
let baseball = L.map (\(a,b) -> [a,b]) $ zip winPercentage runsPerGame
:t baseball
baseball
let (baseballMean, baseballCovMatrix) = meanCov $ fromLists baseball
baseballMean
baseballCovMatrix 
let (baseballEvalues, baseballEvectors) = eigSH baseballCovMatrix
baseballEvalues
baseballEvectors
let xmean = 4.0667
let ymean = 0.5
let samplePoints = [3.3, 3.4 .. 4.7]
let firstSlope = (-7.085537941692915e-2) / (-0.9974865990115773)
let secondSlope = (-0.9974865990115773) / (7.085537941692915e-2)
let firstIntercept = ymean - (xmean * firstSlope)
let secondIntercept = ymean - (xmean * secondSlope)
let firstEigenline = zip samplePoints $ L.map (\x -> x*firstSlope + firstIntercept) samplePoints
let secondEigenline = zip samplePoints $ L.map (\x -> x*secondSlope + secondIntercept) samplePoints
import Graphics.EasyPlot
plot (PNG "runs_and_wins_with_eigenlines.png") [Data2D [Title "Runs Per Game VS Win % in 2014"] [] (zip runsPerGame winPercentage), Data2D [Title "First Eigenline", Style Lines, Color Blue] [] firstEigenline, Data2D [Title "Second Eigenline", Style Lines, Color Red] [] secondEigenline]
plot (PNG "runs_and_wins_first_eigenline.png") [Data2D [Title "Runs Per Game VS Win % in 2014"] [] (zip runsPerGame winPercentage), Data2D [Title "First Eigenline", Style Lines, Color Blue] [] firstEigenline]
let pcaMatrix = principalComponentAnalysis wordsMatrix 5
genericIndex wordsMatrix 22951
findClosestFeaturesToRow pcaMatrix 22951 6
genericIndex wordsMatrix 8020
genericIndex wordsMatrix 13579
genericIndex wordsMatrix 19982
genericIndex wordsMatrix 11178
genericIndex wordsMatrix 22227
-}

module LearningDataAnalysis08 where

import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.Algorithms
import Numeric.LinearAlgebra.HMatrix
import Data.List as L

stopWords :: [String]
stopWords = ["a", "about", "above", "above", "across", "after", "afterwards", "again", "against", "all", "almost", "alone", "along", "already", "also","although","always","am","among", "amongst", "amoungst", "amount",  "an", "and", "another", "any","anyhow","anyone","anything","anyway", "anywhere", "are", "around", "as",  "at", "back","be","became", "because","become","becomes", "becoming", "been", "before", "beforehand", "behind", "being", "below", "beside", "besides", "between", "beyond", "bill", "both", "bottom","but", "by", "call", "can", "cannot", "cant", "co", "con", "could", "couldnt", "cry", "de", "describe", "detail", "do", "done", "dont", "down", "due", "during", "each", "eg", "eight", "either", "eleven","else", "elsewhere", "empty", "enough", "etc", "even", "ever", "every", "everyone", "everything", "everywhere", "except", "few", "fifteen", "fify", "fill", "find", "fire", "first", "five", "for", "former", "formerly", "forty", "found", "four", "from", "front", "full", "further", "get", "give", "go", "got", "had", "has", "hasnt", "have", "he", "hence", "her", "here", "hereafter", "hereby", "herein", "hereupon", "hers", "herself", "him", "himself", "his", "how", "however", "hundred", "i", "ie", "if", "im", "in", "inc", "indeed", "interest", "into", "is", "it", "its", "itself", "just", "keep", "last", "latter", "latterly", "least", "less", "ltd", "made", "many", "may", "me", "meanwhile", "might", "mill", "mine", "more", "moreover", "most", "mostly", "move", "much", "must", "my", "myself", "name", "namely", "neither", "need", "never", "nevertheless", "next", "nine", "no", "nobody", "none", "noone", "nor", "not", "nothing", "now", "nowhere", "of", "off", "often", "on", "once", "one", "only", "onto", "or", "other", "others", "otherwise", "our", "ours", "ourselves", "out", "over", "own","part", "per", "perhaps", "please", "put", "rather", "re", "same", "see", "seem", "seemed", "seeming", "seems", "serious", "several", "she", "should", "show", "side", "since", "sincere", "six", "sixty", "so", "some", "somehow", "someone", "something", "sometime", "sometimes", "somewhere", "still", "such", "system", "take", "ten", "than", "that", "the", "their", "them", "themselves", "then", "thence", "there", "thereafter", "thereby", "therefore", "therein", "thereupon", "these", "they", "thickv", "thin", "third", "this", "those", "though", "three", "through", "throughout", "thru", "thus", "to", "together", "too", "top", "toward", "towards", "twelve", "twenty", "two", "un", "under", "until", "up", "upon", "us", "very", "via", "want", "was", "we", "well", "were", "what", "whatever", "when", "whence", "whenever", "where", "whereafter", "whereas", "whereby", "wherein", "whereupon", "wherever", "whether", "which", "while", "whither", "who", "whoever", "whole", "whom", "whose", "why", "will", "with", "within", "without", "would", "yet", "you", "your", "youre", "yours", "yourself", "yourselves", "the", "rt", "mt", "u"]

{-
    Given a dataset and an integer, performs PCA on that dataset
    using the top eigenvalues specified by the integer.
-}
principalComponentAnalysis :: [[Double]] -> Integer -> Matrix Double
principalComponentAnalysis records top =
      tr $ mul (tr topOrderedEvectors) (tr featureVectors)
  where
      featureVectors = fromLists records
      (_, covMatrix) = meanCov featureVectors
      (_, evectors) = eigSH covMatrix
      topOrderedEvectors = takeColumns (fromInteger top) evectors

{-
    Given a matrix and a row, returns the Euclidean Squared from that row
    to every row in the matrix.
-}
euclideanDistanceSqrdFromRow :: Matrix Double -> Integer -> Vector Double
euclideanDistanceSqrdFromRow records row =
      sumOfSquaresVector
  where
      d = cols records
      copyMatrix = repmat (subMatrix (fromIntegral row, 0) (1, d) records) (rows records) 1
      diffMatrix = records - copyMatrix
      diffSqrdMatrix = diffMatrix * diffMatrix
      sumOfSquaresVector = sum $ toColumns diffSqrdMatrix

{-
    Requires an unsorted list and a nubsorted list. For each value in the
    nubSorted list, we return a list of all index positions in the unsorted
    list which match this position using the elemIndicies function.
-}
order :: Eq a => [a] -> [a] -> [Integer]
order unsorted nubSorted = concatMap (\x -> L.map toInteger $ L.elemIndices x unsorted) nubSorted

{-
    Given a matrix, a row within that matrix, and an integer knn, returns the
    k-Nearest Neighbors to the given row in the given matrix based on Euclidean
    distance squared.
-}
findClosestFeaturesToRow :: Matrix Double -> Integer -> Integer -> [Integer]
findClosestFeaturesToRow records row knn =
      take (fromIntegral knn) orderOfFeatures
  where
      sumOfSquares = toList $ euclideanDistanceSqrdFromRow records row
      orderOfFeatures = order sumOfSquares (nub $ sort sumOfSquares)
