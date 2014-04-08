module HomMad.AI where

import HomMad.Goban
import System.Random

randomSeq :: Int -> [Int]
randomSeq seed = randomRs (0, maxBound) (mkStdGen seed)

move :: [Int] -> GameStatus -> GameStatus
move = undefined

winningRate :: [Int] -> GameStatus -> Point -> (Rational, [Int])
winningRate = undefined

playout :: [Int] -> GameStatus -> (Board, [Int])
playout (r:rs) st = if null candidates
                    then (_board st, rs)
                    else let c = candidates !! (r `mod` length candidates)
                         in playout rs $ putStone st c
    where candidates = pointsCanPut st
playout [] _ = error "playout"

pointsCanPut :: GameStatus -> [Point]
pointsCanPut st = filter (canPut st) allPoints

count :: Board -> (Int, Int)
count b = (count' B, count' W)
    where count' c = length $ filter (areaOf c) allPoints
          areaOf c pt = stone c pt || eye c pt
          stone c pt = boardRef b pt == c
          eye c pt = c `elem` [boardRef b p | p <- aroundOf pt]

allPoints :: [Point]
allPoints = [(row, col) | row <- [0..boardSize-1], col <- [0..boardSize-1]]
