{-
Module      : LearningDataAnalysis06
Description : Contains functions for performing linear regression.
Maintainer  : jcchurch@gmail.com

All GHCi examples used in this chapter:

:l LearningDataAnalysis06
queryDatabase "winloss.sql" "SELECT COUNT(*) FROM winloss"
queryDatabase "winloss.sql" "SELECT COUNT(*) FROM winloss WHERE awayscore == homescore;"
homeRecord <- queryDatabase "winloss.sql" "SELECT homeTeam, SUM(homescore > awayscore), SUM(homescore), COUNT(*) FROM winloss GROUP BY homeTeam;"
awayRecord <- queryDatabase "winloss.sql" "SELECT awayTeam, SUM(awayscore > homescore), SUM(awayscore), COUNT(*) FROM winloss GROUP BY awayTeam;"
let totalWins = zipWith (+) (readDoubleColumn homeRecord 1) (readDoubleColumn awayRecord 1)
let totalRuns = zipWith (+) (readDoubleColumn homeRecord 2) (readDoubleColumn awayRecord 2)
let totalGames = zipWith (+) (readDoubleColumn homeRecord 3) (readDoubleColumn awayRecord 3)
let winPercentage = zipWith (/) totalWins totalGames
let runsPerGame = zipWith (/) totalRuns totalGames
any (\xi -> abs((xi - average runsPerGame) / standardDeviation runsPerGame) > 3) runsPerGame
any (\xi -> abs( (xi - average winPercentage) / standardDeviation winPercentage) > 3) winPercentage
import Graphics.EasyPlot
plot (PNG "runs_and_wins.png") $ Data2D [Title "Runs Per Game VS Win % in 2014"] [] $ zip runsPerGame winPercentage
let (gradient, intercept) = linearRegression runsPerGame winPercentage
gradient
intercept
3*gradient+intercept
4*gradient+intercept
5*gradient+intercept
let winEstimate = map (\x -> x*gradient + intercept) [3.3, 3.4 .. 4.7]
let regressionLine = zip [3.3, 3.4 .. 4.7] winEstimate
plot (PNG "runs_and_wins_with_regression.png") [Data2D [Title "Runs Per Game VS Win % in 2014"] [] (zip runsPerGame winPercentage), Data2D [Title "Regression Line", Style Lines, Color Blue] [] regressionLine]

-}

module LearningDataAnalysis06 where

import Data.List
import Graphics.EasyPlot
import LearningDataAnalysis02
import LearningDataAnalysis04
import LearningDataAnalysis05

{-|
    Given a independent X variable and a dependent Y,
    returns the covariance score.
-}
covariance :: [Double] -> [Double] -> Double
covariance x y = average $ zipWith (\xi yi -> (xi-xavg) * (yi-yavg)) x y
  where
      xavg = average x
      yavg = average y

{-|
    Given a independent X variable and a dependent Y,
    returns the Pearson R Score.
-}
pearsonR :: [Double] -> [Double] -> Double
pearsonR x y = r
  where
      xstdev = standardDeviation x
      ystdev = standardDeviation y
      r = covariance x y / (xstdev * ystdev)

{-|
    Given a independent X variable and a dependent Y,
    returns the Pearson R-Squared Score.
-}
pearsonRsqrd :: [Double] -> [Double] -> Double
pearsonRsqrd x y = pearsonR x y ^ 2

{-|
    Given a independent X variable and a dependent Y,
    returns a tuple with the first value representing
    the gradient of the regression and the second
    value representing the y-intercept.
-}
linearRegression :: [Double] -> [Double] -> (Double, Double)
linearRegression x y = (gradient, intercept)
  where
      xavg = average x
      yavg = average y
      xstdev = standardDeviation x
      gradient = covariance x y / (xstdev * xstdev)
      intercept = yavg - gradient * xavg
