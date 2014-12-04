module HomMad.AI where

import HomMad.Goban
import System.Random
import Data.Ratio (Ratio, (%))
import Data.List (maximumBy)
import Data.Ord (comparing)
import Control.Applicative ((<$>), (<*>))

komi :: Int
komi = 7

randomSeq :: Int -> [Int]
randomSeq seed = randomRs (0, maxBound) (mkStdGen seed)

move :: Int -> GameStatus -> Maybe Coord
move seed st = selectCandidates candidates
    where candidates = filter (canPut st) allCoords
          selectCandidates [] = Nothing
          selectCandidates _  = Just result
          candsAndSeeds = zip candidates $ randomSeq seed
          getRate (cand, s) = winningRateOfACoord s 100 st cand
          (result, _) = maximumBy (comparing getRate) candsAndSeeds

winningRateOfACoord :: Int -> Int -> GameStatus -> Coord -> Ratio Int
winningRateOfACoord seed n st pt =
    (length $ filter (win $ _turn st) results) % n
    where results = map count $ playoutsOfACoord seed n st pt
          win Black (b, w) = b > w + komi
          win White (b, w) = b <= w + komi

playoutsOfACoord :: Int -> Int -> GameStatus -> Coord -> [GameStatus]
playoutsOfACoord seed n st pt = map (flip playout st') seeds
    where seeds = take n $ randomSeq seed
          st' = putStone st pt

playout :: Int -> GameStatus -> GameStatus
playout seed = playout' False $ randomSeq seed
    where
      playout' passed (r:rs) st =
          let candidates = pointsCanPut st
              cand = candidates !! (r `mod` length candidates) in
          if null candidates
          then if passed
               then st
               else playout' True (r:rs) (pass st)
          else playout' False rs $ putStone st cand
      playout' _ [] _ = error "playout"

pointsCanPut :: GameStatus -> [Coord]
pointsCanPut st@GameStatus{_turn=c} =
    filter (\p -> canPut st p && not (isEye st p)) allCoords

isEye :: GameStatus -> Coord -> Bool
isEye st@GameStatus{_board=b, _turn=c} p =
    boardRef b p == Empty &&
    all ((||) <$> isOutOfBoard <*> isLivingChain) (aroundOf p)
    where
      isOutOfBoard = (== OutOfBoard) . boardRef b
      isLivingChain = (&&) <$>
                      (== Point c) . boardRef b <*>
                      (>1) . numOfLiberties . getChain st

count :: GameStatus -> (Int, Int)
count GameStatus{_board=b} = (count' Black, count' White)
    where count' c = length $ filter (areaOf c) allCoords
          areaOf c pt = stone c pt || eye c pt
          stone c pt = boardRef b pt == Point c
          eye c pt = boardRef b pt == Empty &&
                     Point c `elem` [boardRef b p | p <- aroundOf pt]

allCoords :: [Coord]
allCoords = [coord (r, c) | r <- [1..boardSize], c <- [1..boardSize]]
