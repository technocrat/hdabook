{-|
Module      : LearningDataAnalysis04
Description : Contains functions for the purpose of
              demonstrating chart plotting using stock
              market data.
Maintainer  : jcchurch@gmail.com

To install the EasyPlot library, use the cabal command:

cabal install easyplot

You must install GNUplot on your own.

All GHCi examples used in this chapter:

import Graphics.EasyPlot
:l LearningDataAnalysis04
convertCSVFileToSQL "aapl.csv" "aapl.sql" "aapl" ["date STRING", "open REAL", "high REAL", "low REAL", "close REAL", "volume REAL", "adjclose REAL"]
aapl <- pullStockClosingPrices "aapl.sql" "aapl"
plot (PNG "aapl.png") $ Data2D [Title "AAPL"] [] $ aapl
plot (PNG "aapl_line.png") $ Data2D [Title "AAPL", Style Lines] [] $ aapl
let aapl252 = take 252 aapl
let aapl252pc = applyPercentChangeToData aapl252
plot (PNG "aapl_oneyear_pc.png") $ Data2D [Title "AAPL - One Year, % Change", Style Lines] [] $ aapl252pc
convertCSVFileToSQL "googl.csv" "googl.sql" "googl" ["date STRING", "open REAL", "high REAL", "low RDEAL", "close REAL", "volume REAL", "adjclose REAL"]
googl <- pullStockClosingPrices "googl.sql" "googl"
let googl252 = take 252 googl
let googl252pc = applyPercentChangeToData googl252
convertCSVFileToSQL "msft.csv" "msft.sql" "msft" ["date STRING", "open REAL", "high REAL", "low REAL", "close REAL", "volume REAL", "adjclose REAL"]
msft <- pullStockClosingPrices "msft.sql" "msft"
let msft252 = take 252 msft
let msft252pc = applyPercentChangeToData msft252
plot (PNG "aapl_googl_msft_pc.png") [Data2D [Title "AAPL - One Year, % Change", Style Lines, Color Red] [] aapl252pc, Data2D [Title "GOOGL - One Year, % Change", Style Lines, Color Blue] [] googl252pc, Data2D [Title "MSFT - One Year, % Change", Style Lines, Color Green] [] msft252pc]
aapl <- pullStockClosingPrices "aapl.sql" "aapl"
let aapl252 = take 252 aapl
let aapl252pc = applyPercentChangeToData aapl252
let aapl252ma20 = applyMovingAverageToData aapl252pc 20
plot (PNG "aapl_20dayma.png") [Data2D [Title "AAPL - One Year, % Change", Style Lines, Color Red] [] aapl252pc, Data2D [Title "AAPL 20-Day MA", Style Lines, Color Black] [] aapl252ma20 ]
convertCSVFileToSQL "all_month.csv" "earthquakes.sql" "oneMonth" ["time TEXT", "latitude REAL", "longitude REAL", "depth REAL", "mag REAL", "magType TEXT", "nst INTEGER", "gap REAL", "dmin REAL", "rms REAL", "net REAL", "id TEXT", "updated TEXT", "place TEXT", "type TEXT"]
earthquakeCoordinates <- pullLatitudeLongitude "earthquakes.sql" "oneMonth"
coords <- pullLatitudeLongitude "earthquakes.sql" "oneMonth"
plot (PNG "earthquakes.png") [Data2D [Title "Earthquakes", Color Red, Style Dots] [] coords ]

-}

module LearningDataAnalysis04 where

import Data.List
import Database.HDBC.Sqlite3
import Database.HDBC
import LearningDataAnalysis02

{-|
    Given a 2D list of SqlValue values, parses and returns
    the integers found in one specified column.
-}
readIntegerColumn :: [[SqlValue]] -> Integer-> [Integer]
readIntegerColumn sqlResult index =
      map (\row -> fromSql $ genericIndex row index :: Integer) sqlResult

{-|
    Given a 2D list of SqlValue values, parses and returns
    the doubles found in one specified column.
-}
readDoubleColumn :: [[SqlValue]] -> Integer -> [Double]
readDoubleColumn sqlResult index =
      map (\row -> fromSql $ genericIndex row index :: Double) sqlResult

{-|
    Given a 2D list of SqlValue values, parses and returns
    the strings found in one specified column.
-}
readStringColumn :: [[SqlValue]] -> Integer -> [String]
readStringColumn sqlResult index =
      map (\row -> fromSql $ genericIndex row index :: String) sqlResult

{-|
    Given a filename to a Sqlite3 database and a SQL query,
    returns the results of that query.
-}
queryDatabase :: FilePath -> String -> IO [[SqlValue]]
queryDatabase databaseFile sqlQuery = do
      conn <- connectSqlite3 databaseFile
      result <- quickQuery' conn sqlQuery []
      disconnect conn
      return result

{-|
    Given a filename to a Sqlite3 database and table name,
    returns the row id and the adjusted close of that row id.
-}
pullStockClosingPrices :: FilePath -> String -> IO [(Double, Double)]
pullStockClosingPrices databaseFile tablename = do
      sqlResult <- queryDatabase databaseFile ("SELECT rowid,adjclose FROM "++tablename)
      return $ zip (reverse $ readDoubleColumn sqlResult 0) (readDoubleColumn sqlResult 1)

{-|
    Given a value and first value starting point, scales the value
    to a percentage of the first value.
-}
percentChange :: Double -> Double -> Double
percentChange value first = 100.0 * (value - first) / first

{-|
    Given a list of row ids and adjusted close values, reverses that list
    and scales the data based on percentChange.
-}
applyPercentChangeToData :: [(Double, Double)] -> [(Double, Double)]
applyPercentChangeToData dataset = zip indicies scaledData
  where
      (_, first) = last dataset
      indicies = reverse [1.0 .. (genericLength dataset)]
      scaledData = map (\(_, value) -> percentChange value first) dataset

{-|
    Given a list of values and an window size,
    returns a moving average of the data where
    each data point returns is the average of
    window size values.
-}
movingAverage :: [Double] -> Integer -> [Double]
movingAverage values window
 | window >= genericLength values = [ average values ]
 | otherwise = average (genericTake window values):movingAverage (tail values) window

{-|
    Given a list of tuples consisting of row id and close
    and a window size, applies the moving average to the close values.
-}
applyMovingAverageToData :: [(Double, Double)] -> Integer ->  [(Double, Double)]
applyMovingAverageToData dataset window =
      zip [fromIntegral window..] $ movingAverage (map snd (reverse dataset)) window 

{-|
    Given a file name of a US Geological Society database and a table name,
    returns the latitude and longitude coordinates of each earthquake
    in that database.
-}
pullLatitudeLongitude :: FilePath -> String -> IO [(Double, Double)]
pullLatitudeLongitude databaseFile tablename = do
      sqlResult <- queryDatabase databaseFile ("SELECT latitude,longitude FROM "++tablename)
      return $ zip (readDoubleColumn sqlResult 1) (readDoubleColumn sqlResult 0)
