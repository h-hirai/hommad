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

opponent :: Color -> Color
opponent Black = White
opponent White = Black

type Board a = IntMap a

type Coord = (Int, Int)

data GameStatus = GameStatus {
      _board :: Board Color    -- ^Board status
    , _turn :: Color           -- ^Turn
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
initGame = GameStatus emptyBoard Black Nothing

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
canPut GameStatus{_board=b, _turn=t, _ko=ko} pt =
    isEmpty && isNotKo ko && (hasLiberty || canKillOpponet)
    where isEmpty = boardRef b pt == Empty
          isNotKo (Just koPt) = koPt /= pt
          isNotKo Nothing     = True
          newBoard = boardPut t b pt
          newChain = getChain newBoard pt
          hasLiberty = isAlive newChain
          canKillOpponet = F.any (not . isAlive . getChain newBoard) $
                           _chainOpponents newChain

putStone :: GameStatus -> Coord -> GameStatus
putStone (GameStatus b t _) pt = GameStatus newBoard (opponent t) ko
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

pass :: GameStatus -> GameStatus
pass st@GameStatus{_turn=t} = st{_turn=opponent t}
