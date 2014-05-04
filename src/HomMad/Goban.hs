module HomMad.Goban where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (foldl')
import qualified Data.Foldable as F

boardSize :: Int
boardSize = 9

-- |Status of a point on a board.
data Point a = Point a
             | Empty
             | OutOfBoard
    deriving (Show, Eq, Ord)

data Color = Black
           | White
    deriving (Show, Eq, Ord)

type Board a = IntMap a

type Coord = (Int, Int)

data GameStatus = GameStatus {
      _board :: Board Color    -- ^Board status
    , _turn :: Color           -- ^Turn
    , _prisonersB :: Int       -- ^Captured by Black
    , _prisonersW :: Int       -- ^Captured by White
    , _ko :: Maybe Coord       -- ^Ko
    } deriving (Show, Eq)

data Chain = Chain {
      _chainCoords :: Set Coord -- ^Coords of the chain stones
    , _chainLiberties :: Set Coord -- ^Coords of the chain liberties
    , _chainOpponents :: Set Coord -- ^Coords of the contacting opponents
    } deriving (Show, Eq, Ord)

emptyBoard :: Board a
emptyBoard = IM.empty

initGame :: GameStatus
initGame = GameStatus emptyBoard Black 0 0 Nothing

-- |
-- >>> boardRef emptyBoard (1,-1)
-- O
-- >>> boardRef emptyBoard (boardSize,1)
-- O

boardRef :: Board a -> Coord -> Point a
boardRef b (row, col)
    | row < 0 = OutOfBoard
    | row >= boardSize = OutOfBoard
    | col < 0 = OutOfBoard
    | col >= boardSize = OutOfBoard
    | otherwise = maybe Empty Point $ IM.lookup (row*boardSize + col) b

boardPut :: a -> Board a -> Coord -> Board a
boardPut a b (row, col) = IM.insert (row*boardSize + col) a b

boardRemove :: Board a -> Coord -> Board a
boardRemove b (row, col) = IM.delete (row*boardSize + col) b

-- |
-- >>> aroundOf (0,0)
-- [(-1,0),(1,0),(0,-1),(0,1)]

aroundOf :: Coord -> [Coord]
aroundOf (row, col) = [(row-1, col), (row+1, col), (row, col-1), (row, col+1)]

getChain :: Board Color -> Coord -> Chain
getChain b pt = case color of
                 Empty -> Chain S.empty S.empty S.empty
                 OutOfBoard -> Chain S.empty S.empty S.empty
                 _ -> getCoords (Chain S.empty S.empty S.empty) pt
    where
      color = boardRef b pt
      getCoords ch@(Chain ps ls os) p =
          case boardRef b p of
            Empty -> ch{_chainLiberties=S.insert p ls}
            OutOfBoard -> ch
            c | c == color && not (p `S.member` ps) ->
                  foldl' getCoords ch{_chainCoords=S.insert p ps} (aroundOf p)
              | c /= color -> ch{_chainOpponents=S.insert p os}
              | otherwise -> ch

isAlive :: Chain -> Bool
isAlive Chain{_chainLiberties=ls} = not (S.null ls)

canPut :: GameStatus -> Coord -> Bool
canPut st pt = isEmpty && isNotKo (_ko st) && hasLibertyOrCanKill
    where isEmpty = boardRef (_board st) pt == Empty
          isNotKo (Just koPt) = koPt /= pt
          isNotKo Nothing     = True
          hasLibertyOrCanKill = isAlive $ getChain (_board $ putStone st pt) pt

putStone :: GameStatus -> Coord -> GameStatus
putStone (GameStatus b t pb pw _) pt = next t
    where
      newBoard' = boardPut t b pt
      newBoard = S.foldl' boardRemove newBoard' captured
      chain = getChain newBoard' pt
      captured = F.foldMap (\p -> let ch = getChain newBoard' p in
                             if isAlive ch then S.empty else _chainCoords ch)
                 (_chainOpponents chain)
      ko = if S.size captured == 1 &&
              S.size (_chainCoords chain) == 1 &&
              not (isAlive chain)
           then Just $ S.toList captured !! 0
           else Nothing
      next Black = GameStatus newBoard White (pb + S.size captured) pw ko
      next White = GameStatus newBoard Black pb (pw + S.size captured) ko

pass :: GameStatus -> GameStatus
pass st@GameStatus{_turn=Black} = st{_turn=White}
pass st@GameStatus{_turn=White} = st{_turn=Black}
