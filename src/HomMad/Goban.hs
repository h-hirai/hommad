module HomMad.Goban where

import Data.Monoid
import Data.Vector (Vector, unsafeIndex)
import qualified Data.Vector as V (replicate, update, singleton)
import Data.Set (Set, (\\))
import qualified Data.Set as S
import qualified Data.Foldable as F
import Data.List (foldl', partition)

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

type Board a = Vector (Point a)

type Coord = (Int, Int)

toIdx :: Coord -> Int
toIdx (row, col) = row*boardSize + col

data GameStatus = GameStatus {
      _board :: Board Color    -- ^Board status
    , _turn :: Color           -- ^Turn
    , _ko :: Maybe Coord       -- ^Ko
    , _chains :: Board Chain
    } deriving (Show, Eq)

data Chain = Chain {
      _chainSize :: Int
    , _chainCoords :: Set Coord -- ^Coords of the chain stones
    , _chainLiberties :: Set Coord -- ^Coords of the chain liberties
    } deriving (Show, Eq, Ord)

instance Monoid Chain where
    mempty = Chain 0 S.empty S.empty
    mappend c1 c2
        | c1 == c2 = c1
        | c1 == mempty = c2
        | c2 == mempty = c1
        | otherwise = Chain newSize coords liberties
      where
        coords = _chainCoords c1 `S.union` _chainCoords c2
        newSize = _chainSize c1 + _chainSize c2
        liberties = _chainLiberties c1 `S.union` _chainLiberties c2 \\ coords

emptyBoard :: Board a
emptyBoard = V.replicate (boardSize * boardSize) Empty

initGame :: GameStatus
initGame = GameStatus emptyBoard Black Nothing emptyBoard

-- |
-- >>> boardRef emptyBoard (1,-1)
-- O
-- >>> boardRef emptyBoard (boardSize,1)
-- O

boardRef :: Board a -> Coord -> Point a
boardRef b pt@(row, col)
    | row < 0 = OutOfBoard
    | row >= boardSize = OutOfBoard
    | col < 0 = OutOfBoard
    | col >= boardSize = OutOfBoard
    | otherwise = unsafeIndex b (toIdx pt)

boardPut :: a -> Board a -> Coord -> Board a
boardPut a b pt = V.update b (V.singleton (toIdx pt, Point a))

boardRemove :: Board a -> Coord -> Board a
boardRemove b pt = V.update b (V.singleton (toIdx pt, Empty))

-- |
-- >>> aroundOf (0,0)
-- [(-1,0),(1,0),(0,-1),(0,1)]

aroundOf :: Coord -> [Coord]
aroundOf (row, col) = [(row-1, col), (row+1, col), (row, col-1), (row, col+1)]

getChain :: GameStatus -> Coord -> Chain
getChain st pt = maybe mempty id $
                 case boardRef (_chains st) pt of
                   (Point ch) -> Just ch
                   _ -> Nothing

filterNeighbor :: Eq a => Board a -> Point a -> Coord -> [Coord]
filterNeighbor b p coord = filter ((==p).boardRef b) $ aroundOf coord

isLastLiberty :: Coord -> Chain -> Bool
isLastLiberty pt = (==S.singleton pt) . _chainLiberties

canPut :: GameStatus -> Coord -> Bool
canPut st@GameStatus{_board=b, _turn=t, _ko=ko} pt =
    isEmpty && isNotKo ko && (hasLiberty || canKillOpponet)
    where
      isEmpty = boardRef b pt == Empty
      isNotKo (Just koPt) = koPt /= pt
      isNotKo Nothing     = True
      neighborEmpty = filterNeighbor b Empty pt
      neighborSame = filterNeighbor b (Point t) pt
      neighborOpponent = filterNeighbor b (Point $ opponent t) pt
      hasLiberty = not (null neighborEmpty) ||
                   any (not.isLastLiberty pt) (map (getChain st) neighborSame)
      canKillOpponet =
          any (isLastLiberty pt) $ map (getChain st) neighborOpponent

updateChain :: (Chain -> Chain) -> Coord -> Board Chain -> Board Chain
updateChain f pt chMap =
    case boardRef chMap pt of
      Point ch -> let ch' = f ch in
                  S.foldl' (boardPut ch') chMap $ _chainCoords ch'
      _ -> chMap

removeChainFromMap :: Set Coord -> Board Chain -> Board Chain
removeChainFromMap removed chMap = S.foldl' addLiberty removedMap removed
    where
      removedMap = S.foldl' boardRemove chMap removed
      addLiberty :: Board Chain -> Coord -> Board Chain
      addLiberty cm pt = foldl' (addLiberty' pt) cm $ aroundOf pt
      addLiberty' :: Coord -> Board Chain ->  Coord -> Board Chain
      addLiberty' l cm pt =
          updateChain (\ch@Chain{_chainLiberties=ls} ->
                       ch{_chainLiberties=S.insert l ls}) pt cm

removeLiberty :: Coord -> [Chain] -> Board Chain -> Board Chain
removeLiberty pt cs chMap = foldl' update chMap cs
    where
      update :: Board Chain -> Chain -> Board Chain
      update cm ch@Chain{_chainLiberties=ls} =
          let ch' = ch{_chainLiberties=S.delete pt ls} in
          S.foldl' (boardPut ch') cm $ _chainCoords ch

updateChainMap :: Coord -> [Chain] ->Chain -> Set Coord ->
                  Board Chain -> Board Chain
updateChainMap pt cs ch@Chain{_chainCoords=addend} omitted chMap =
    removeChainFromMap omitted $
    removeLiberty pt cs $
    S.foldl' (boardPut ch) chMap addend

putStone :: GameStatus -> Coord -> GameStatus
putStone st@(GameStatus b t _ cs) pt =
    GameStatus newBoard (opponent t) ko newChainMap
    where
      neighborSame = filterNeighbor b (Point t) pt
      neighborOpponent = filterNeighbor b (Point $ opponent t) pt
      neighborEmpty = filterNeighbor b Empty pt
      chainSingleton = Chain 1 (S.singleton pt) (S.fromList neighborEmpty)
      chainConnected =
          mconcat $ chainSingleton : map (getChain st) neighborSame
      (chainCaptured, chainOpponent) =
          partition (isLastLiberty pt) $ map (getChain st) neighborOpponent
      captured = F.foldMap _chainCoords chainCaptured
      newBoard = S.foldl' boardRemove (boardPut t b pt) captured
      ko = if S.size captured == 1 &&
              _chainSize chainConnected == 1 &&
              (S.null $ _chainLiberties chainConnected)
           then Just $ S.toList captured !! 0
           else Nothing
      newChainMap = updateChainMap pt chainOpponent chainConnected captured cs

pass :: GameStatus -> GameStatus
pass st@GameStatus{_turn=t} = st{_turn=opponent t, _ko=Nothing}
