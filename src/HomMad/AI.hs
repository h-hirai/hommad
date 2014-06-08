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
    filter (\p -> canPut st p &&
                  not (isSimpleEye st c p) &&
                  not (isCombinedEye st c p)) allCoords

isSingleSpace :: Board Color -> Color -> Coord -> Bool
isSingleSpace b c p =
    boardRef b p == Empty &&
    all ((\n -> n == Point c || n == OutOfBoard) . boardRef b) (aroundOf p)

chainsSurrounding :: GameStatus -> Color -> Coord -> Set Chain
chainsSurrounding st@GameStatus{_board=b} c p =
    S.fromList $
    map (getChain st) $ filter ((== Point c) . boardRef b) $ aroundOf p

isSimpleEye :: GameStatus -> Color -> Coord -> Bool
isSimpleEye st@GameStatus{_board=b} c p =
    isSingleSpace b c p &&
    let (p1:rest) = filter ((==Point c).(boardRef b)) $ aroundOf p
        ch = _chainCoords $ getChain st p1 in
    all (`S.member` ch) rest

isCombinedEye :: GameStatus -> Color -> Coord -> Bool
isCombinedEye st@GameStatus{_board=b} c p =
    isSingleSpace b c p &&
    S.size chains == 2 &&
    F.any isOtherEye (_chainLiberties $ S.findMax chains)
    where chains = chainsSurrounding st c p
          isOtherEye pt = pt /= p &&
                          isSingleSpace b c pt &&
                          chainsSurrounding st c pt == chains

count :: GameStatus -> (Int, Int)
count GameStatus{_board=b} = (count' Black, count' White)
    where count' c = length $ filter (areaOf c) allCoords
          areaOf c pt = stone c pt || eye c pt
          stone c pt = boardRef b pt == Point c
          eye c pt = boardRef b pt == Empty &&
                     Point c `elem` [boardRef b p | p <- aroundOf pt]

allCoords :: [Coord]
allCoords = [Coord row col | row <- [0..boardSize-1], col <- [0..boardSize-1]]
