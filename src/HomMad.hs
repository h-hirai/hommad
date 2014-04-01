module HomMad where

import Data.Set (Set)
import qualified Data.Set as S
import Data.List (foldl')

boardSize :: Int
boardSize = 9

-- |Status of a point on a board.
data Color = B -- ^Black
           | W -- ^White
           | E -- ^Empty
           | O -- ^Out Of Boad
    deriving (Show, Eq)

type Board = [[Color]]

type Point = (Int, Int)

data GameStatus = GameStatus {
      _board :: Board          -- ^Board status
    , _turn :: Color           -- ^Turn
    , _prisonersB :: Int       -- ^Captured by Black
    , _prisonersW :: Int       -- ^Captured by White
    , _ko :: Maybe Point       -- ^Ko
    } deriving (Show, Eq)

data Chain = Chain {
      _chainColor :: Color      -- ^Chain color
    , _chainPoints :: Set Point -- ^Points of the chain stones
    , _chainLiberties :: Set Point -- ^Points of the chain liberties
    , _chainOpponents :: Set Point -- ^Points of the contacting opponents
    } deriving (Show, Eq)

emptyBoard :: Board
emptyBoard = replicate boardSize $ replicate boardSize E

initGame :: GameStatus
initGame = GameStatus emptyBoard B 0 0 Nothing

-- |
-- >>> boardRef emptyBoard (1,-1)
-- O
-- >>> boardRef emptyBoard (boardSize,1)
-- O

boardRef :: Board -> Point -> Color
boardRef b (row, col) | row < 0 = O
                      | row >= boardSize = O
                      | col < 0 = O
                      | col >= boardSize = O
                      | otherwise = b !! row !! col

-- |
-- >>> boardRef (boardPut B emptyBoard (3,3)) (3,3)
-- B
-- >>> boardRef (boardPut W emptyBoard (3,3)) (3,3)
-- W
-- >>> boardRef (boardPut W emptyBoard (3,3)) (3,2)
-- E

boardPut :: Color -> Board -> Point -> Board
boardPut c b (row, col) = rplcIdx b row $ rplcIdx (b!!row) col c
    where rplcIdx l n a = let (i, (_:t)) = splitAt n l in i ++ (a:t)

-- |
-- >>> aroundOf (0,0)
-- [(-1,0),(1,0),(0,-1),(0,1)]

aroundOf :: Point -> [Point]
aroundOf (row, col) = [(row-1, col), (row+1, col), (row, col-1), (row, col+1)]

getChain :: Board -> Point -> Chain
getChain b pt = case boardRef b pt of
                 E -> Chain E S.empty S.empty S.empty
                 O -> Chain O S.empty S.empty S.empty
                 c -> getPoints (Chain c S.empty S.empty S.empty) pt
    where
      getPoints ch@(Chain color ps ls os) p =
          case boardRef b p of
            E -> ch{_chainLiberties=S.insert p ls}
            O -> ch
            c | c == color && not (p `S.member` ps) ->
                  foldl' getPoints ch{_chainPoints=S.insert p ps} (aroundOf p)
              | c /= color -> ch{_chainOpponents=S.insert p os}
              | otherwise -> ch

isAlive :: Chain -> Bool
isAlive Chain{_chainLiberties=ls} = not (S.null ls)

canPut :: GameStatus -> Point -> Bool
canPut st pt = boardRef (_board st) pt == E && koCheck (_ko st)
    where koCheck (Just koPt) = koPt /= pt && libertyCheck
          koCheck Nothing     = libertyCheck
          newBoard = boardPut (_turn st) (_board st) pt
          libertyCheck = isAlive (getChain newBoard pt)

putStone :: GameStatus -> Point -> GameStatus
putStone st@GameStatus{_board=b, _turn=t} p =
    st{_board=boardPut t b p, _turn=next t}
    where next B = W
          next W = B
          next _ = error ""
