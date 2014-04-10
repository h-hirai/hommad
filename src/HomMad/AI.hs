module HomMad.AI where

import HomMad.Goban
import System.Random
import qualified Data.Set as S

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

isSingleSpace :: Board -> Color -> Point -> Bool
isSingleSpace b c p =
    boardRef b p == E &&
    all ((\n -> n == c || n == O) . boardRef b) (aroundOf p)

chainsSurrounding :: Board -> Color -> Point -> [Chain]
chainsSurrounding b c p =
    map (getChain b) $ filter ((==c) . boardRef b) $ aroundOf p

isSimpleEye :: Board -> Color -> Point -> Bool
isSimpleEye b c p = isSingleSpace b c p &&
                    let (p1:rest) = filter ((==c).(boardRef b)) $ aroundOf p
                        ch = _chainPoints $ getChain b p1 in
                    all (`S.member` ch) rest

isCombinedEye :: Board -> Color -> Point -> Bool
isCombinedEye b c p = undefined

isEye :: Board -> Color -> Point -> Bool
isEye b c p =  isSimpleEye b c p || isSharedEye
    where isSharedEye = undefined

count :: Board -> (Int, Int)
count b = (count' B, count' W)
    where count' c = length $ filter (areaOf c) allPoints
          areaOf c pt = stone c pt || eye c pt
          stone c pt = boardRef b pt == c
          eye c pt = c `elem` [boardRef b p | p <- aroundOf pt]

allPoints :: [Point]
allPoints = [(row, col) | row <- [0..boardSize-1], col <- [0..boardSize-1]]
