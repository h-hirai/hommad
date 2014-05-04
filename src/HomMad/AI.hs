module HomMad.AI where

import HomMad.Goban
import System.Random
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Foldable as F
import Data.Ratio (Ratio, (%))
import Data.List (maximumBy)
import Data.Ord (comparing)

komi :: Int
komi = 7

randomSeq :: Int -> [Int]
randomSeq seed = randomRs (0, maxBound) (mkStdGen seed)

move :: Int -> GameStatus -> GameStatus
move seed st = putStone st result
    where candidates = filter (canPut st) allCoords
          candsAndSeeds = zip candidates $ randomSeq seed
          getRate (cand, s) = winningRateOfACoord s 100 st cand
          (result, _) = maximumBy (comparing getRate) candsAndSeeds

winningRateOfACoord :: Int -> Int -> GameStatus -> Coord -> Ratio Int
winningRateOfACoord seed n st pt =
    (length $ filter (win $ _turn st) results) % n
    where results = map count $ playoutsOfACoord seed n st pt
          win B (b, w) = b > w + komi
          win W (b, w) = b <= w + komi
          win _ _ = error "winningRateOfACoord"

playoutsOfACoord :: Int -> Int -> GameStatus -> Coord -> [Board]
playoutsOfACoord seed n st pt = map (flip playout st') seeds
    where seeds = take n $ randomSeq seed
          st' = putStone st pt

playout :: Int -> GameStatus -> Board
playout seed = playout' False $ randomSeq seed
    where
      playout' passed (r:rs) st =
          let candidates = pointsCanPut st
              cand = candidates !! (r `mod` length candidates) in
          if null candidates
          then if passed
               then _board st
               else playout' True (r:rs) (pass st)
          else playout' False rs $ putStone st cand
      playout' _ [] _ = error "playout"

pointsCanPut :: GameStatus -> [Coord]
pointsCanPut st@GameStatus{_board=b, _turn=c} =
    filter (\p -> canPut st p &&
                  not (isSimpleEye b c p) &&
                  not (isCombinedEye b c p)) allCoords

isSingleSpace :: Board -> Color -> Coord -> Bool
isSingleSpace b c p =
    boardRef b p == E &&
    all ((\n -> n == c || n == O) . boardRef b) (aroundOf p)

chainsSurrounding :: Board -> Color -> Coord -> Set Chain
chainsSurrounding b c p =
    S.fromList $ map (getChain b) $ filter ((==c) . boardRef b) $ aroundOf p

isSimpleEye :: Board -> Color -> Coord -> Bool
isSimpleEye b c p = isSingleSpace b c p &&
                    let (p1:rest) = filter ((==c).(boardRef b)) $ aroundOf p
                        ch = _chainCoords $ getChain b p1 in
                    all (`S.member` ch) rest

isCombinedEye :: Board -> Color -> Coord -> Bool
isCombinedEye b c p = isSingleSpace b c p &&
                      S.size chains == 2 &&
                      F.any isOtherEye (_chainLiberties $ S.findMax chains)
    where chains = chainsSurrounding b c p
          isOtherEye pt = pt /= p &&
                          isSingleSpace b c pt &&
                          chainsSurrounding b c pt == chains

count :: Board -> (Int, Int)
count b = (count' B, count' W)
    where count' c = length $ filter (areaOf c) allCoords
          areaOf c pt = stone c pt || eye c pt
          stone c pt = boardRef b pt == c
          eye c pt = boardRef b pt == E &&
                     c `elem` [boardRef b p | p <- aroundOf pt]

allCoords :: [Coord]
allCoords = [(row, col) | row <- [0..boardSize-1], col <- [0..boardSize-1]]
