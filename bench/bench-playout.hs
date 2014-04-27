module Main where

import HomMad.Goban (initGame)
import HomMad.AI (winningRateOfAPoint)
import Data.Time.Clock

main :: IO ()
main = do
  start <- getCurrentTime
  putStrLn $ show $ winningRateOfAPoint 0 100 initGame (0, 0)
  end <- getCurrentTime
  putStrLn $ "running playout 100 times took " ++ show (diffUTCTime end start)
