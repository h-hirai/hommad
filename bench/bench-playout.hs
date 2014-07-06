module Main where

import HomMad.Goban (initGame, coord)
import HomMad.AI (winningRateOfACoord)
import Data.Time.Clock

main :: IO ()
main = do
  start <- getCurrentTime
  putStrLn $ show $ winningRateOfACoord 0 100 initGame $ coord (1, 1)
  end <- getCurrentTime
  putStrLn $ "running playout 100 times took " ++ show (diffUTCTime end start)
