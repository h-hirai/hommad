module HomMad.Goban where

import Data.Set (Set)
import qualified Data.Set as S
import Data.List (foldl')
import qualified Data.Foldable as F

boardSize :: Int
boardSize = 9

-- |Status of a point on a board.
data Color = B -- ^Black
           | W -- ^White
           | E -- ^Empty
           | O -- ^Out Of Boad
    deriving (Show, Eq, Ord)

type Board = [[Color]]

type Coord = (Int, Int)

data GameStatus = GameStatus {
      _board :: Board          -- ^Board status
    , _turn :: Color           -- ^Turn
    , _prisonersB :: Int       -- ^Captured by Black
    , _prisonersW :: Int       -- ^Captured by White
    , _ko :: Maybe Coord       -- ^Ko
    } deriving (Show, Eq)

data Chain = Chain {
      _chainColor :: Color      -- ^Chain color
    , _chainCoords :: Set Coord -- ^Coords of the chain stones
    , _chainLiberties :: Set Coord -- ^Coords of the chain liberties
    , _chainOpponents :: Set Coord -- ^Coords of the contacting opponents
    } deriving (Show, Eq, Ord)

emptyBoard :: Board
emptyBoard = replicate boardSize $ replicate boardSize E

initGame :: GameStatus
initGame = GameStatus emptyBoard B 0 0 Nothing

-- |
-- >>> boardRef emptyBoard (1,-1)
-- O
-- >>> boardRef emptyBoard (boardSize,1)
-- O

boardRef :: Board -> Coord -> Color
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

boardPut :: Color -> Board -> Coord -> Board
boardPut c b (row, col) = rplcIdx b row $ rplcIdx (b!!row) col c
    where rplcIdx l n a = let (i, (_:t)) = splitAt n l in i ++ (a:t)

-- |
-- >>> aroundOf (0,0)
-- [(-1,0),(1,0),(0,-1),(0,1)]

aroundOf :: Coord -> [Coord]
aroundOf (row, col) = [(row-1, col), (row+1, col), (row, col-1), (row, col+1)]

getChain :: Board -> Coord -> Chain
getChain b pt = case boardRef b pt of
                 E -> Chain E S.empty S.empty S.empty
                 O -> Chain O S.empty S.empty S.empty
                 c -> getCoords (Chain c S.empty S.empty S.empty) pt
    where
      getCoords ch@(Chain color ps ls os) p =
          case boardRef b p of
            E -> ch{_chainLiberties=S.insert p ls}
            O -> ch
            c | c == color && not (p `S.member` ps) ->
                  foldl' getCoords ch{_chainCoords=S.insert p ps} (aroundOf p)
              | c /= color -> ch{_chainOpponents=S.insert p os}
              | otherwise -> ch

isAlive :: Chain -> Bool
isAlive Chain{_chainLiberties=ls} = not (S.null ls)

canPut :: GameStatus -> Coord -> Bool
canPut st pt = isEmpty && isNotKo (_ko st) && hasLibertyOrCanKill
    where isEmpty = boardRef (_board st) pt == E
          isNotKo (Just koPt) = koPt /= pt
          isNotKo Nothing     = True
          hasLibertyOrCanKill = isAlive $ getChain (_board $ putStone st pt) pt

putStone :: GameStatus -> Coord -> GameStatus
putStone (GameStatus b t pb pw _) pt = next t
    where
      newBoard' = boardPut t b pt
      newBoard = S.foldl' (boardPut E) newBoard' captured
      chain = getChain newBoard' pt
      captured = F.foldMap (\p -> let ch = getChain newBoard' p in
                             if isAlive ch then S.empty else _chainCoords ch)
                 (_chainOpponents chain)
      ko = if S.size captured == 1 &&
              S.size (_chainCoords chain) == 1 &&
              not (isAlive chain)
           then Just $ S.toList captured !! 0
           else Nothing
      next B = GameStatus newBoard W (pb + S.size captured) pw ko
      next W = GameStatus newBoard B pb (pw + S.size captured) ko
      next _ = error "putStone"

pass :: GameStatus -> GameStatus
pass st@GameStatus{_turn=B} = st{_turn=W}
pass st@GameStatus{_turn=W} = st{_turn=B}
pass _ = error "pass"
