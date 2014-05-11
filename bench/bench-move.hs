module Main where

import HomMad.Goban (initGame)
import HomMad.AI (move)
import Data.Time.Clock

main :: IO ()
main = do
  start <- getCurrentTime
  putStrLn $ show $ move 0 initGame
  end <- getCurrentTime
  putStrLn $ "running move for init Game took " ++ show (diffUTCTime end start)
