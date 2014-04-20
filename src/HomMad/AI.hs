module HomMad.AI where

import HomMad.Goban
import System.Random
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Foldable as F

randomSeq :: Int -> [Int]
randomSeq seed = randomRs (0, maxBound) (mkStdGen seed)

move :: [Int] -> GameStatus -> GameStatus
move = undefined

winningRate :: [Int] -> GameStatus -> Point -> (Rational, [Int])
winningRate = undefined

playoutsOfAPoint :: Int -> Int -> GameStatus -> Point -> [Board]
playoutsOfAPoint seed n st pt = map (flip playout st') seeds
    where seeds = take n $ randomSeq seed
          st' = putStone st pt

playout :: Int -> GameStatus -> Board
playout seed = playout' False $ randomSeq seed
    where
      playout' passed (r:rs) st =
          let candidates = pointsCanPut st
              c = candidates !! (r `mod` length candidates) in
          if null candidates
          then if passed
               then _board st
               else playout' True (r:rs) (pass st)
          else playout' False rs $ putStone st c
      playout' _ [] _ = error "playout"

pointsCanPut :: GameStatus -> [Point]
pointsCanPut st@GameStatus{_board=b, _turn=c} =
    filter (\p -> canPut st p &&
                  not (isSimpleEye b c p) &&
                  not (isCombinedEye b c p)) allPoints

isSingleSpace :: Board -> Color -> Point -> Bool
isSingleSpace b c p =
    boardRef b p == E &&
    all ((\n -> n == c || n == O) . boardRef b) (aroundOf p)

chainsSurrounding :: Board -> Color -> Point -> Set Chain
chainsSurrounding b c p =
    S.fromList $ map (getChain b) $ filter ((==c) . boardRef b) $ aroundOf p

isSimpleEye :: Board -> Color -> Point -> Bool
isSimpleEye b c p = isSingleSpace b c p &&
                    let (p1:rest) = filter ((==c).(boardRef b)) $ aroundOf p
                        ch = _chainPoints $ getChain b p1 in
                    all (`S.member` ch) rest

isCombinedEye :: Board -> Color -> Point -> Bool
isCombinedEye b c p = isSingleSpace b c p &&
                      S.size chains == 2 &&
                      F.any isOtherEye (_chainLiberties $ S.findMax chains)
    where chains = chainsSurrounding b c p
          isOtherEye pt = pt /= p &&
                          isSingleSpace b c pt &&
                          chainsSurrounding b c pt == chains

count :: Board -> (Int, Int)
count b = (count' B, count' W)
    where count' c = length $ filter (areaOf c) allPoints
          areaOf c pt = stone c pt || eye c pt
          stone c pt = boardRef b pt == c
          eye c pt = boardRef b pt == E &&
                     c `elem` [boardRef b p | p <- aroundOf pt]

allPoints :: [Point]
allPoints = [(row, col) | row <- [0..boardSize-1], col <- [0..boardSize-1]]
