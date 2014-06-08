module Main where

import HomMad.Goban (initGame, Coord(..))
import HomMad.AI (winningRateOfACoord)
import Data.Time.Clock

main :: IO ()
main = do
  start <- getCurrentTime
  putStrLn $ show $ winningRateOfACoord 0 100 initGame (Coord 0 0)
  end <- getCurrentTime
  putStrLn $ "running playout 100 times took " ++ show (diffUTCTime end start)
