module HomMad.Goban where

import Data.Monoid
import Data.Vector (Vector, unsafeIndex, (//))
import qualified Data.Vector as V (replicate, cons, snoc, concat, update,
                                   singleton, (++))
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

newtype Coord = Coord { toInt::Int } deriving (Eq, Ord)

row :: Coord -> Int
row (Coord pt) = pt `div` (boardSize+2)

col :: Coord -> Int
col (Coord pt) = pt `mod` (boardSize+2)

instance Show Coord where
    show pt = "coord " ++ show (row pt, col pt)

coord :: (Int, Int) -> Coord
coord (r, c) = Coord $ r*(boardSize+2) + c

data GameStatus = GameStatus {
      _board :: Board Color
    , _turn :: Color
    , _ko :: Maybe Coord
    , _chains :: Board Chain
    } deriving (Show, Eq)

data Chain = Chain {
      _chainSize :: Int
    , _chainCoords :: Set Coord
    , _chainLiberties :: Set Coord
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
emptyBoard = V.replicate (boardSize + 2) OutOfBoard V.++
             V.concat (replicate boardSize
                       (OutOfBoard `V.cons`
                        V.replicate boardSize Empty `V.snoc`
                        OutOfBoard)) V.++
             V.replicate (boardSize + 2) OutOfBoard

initGame :: GameStatus
initGame = GameStatus emptyBoard Black Nothing emptyBoard

-- |
-- >>> boardRef emptyBoard (1,0)
-- OutOfBoard
-- >>> boardRef emptyBoard (boardSize+1,1)
-- OutOfBoard

boardRef :: Board a -> Coord -> Point a
boardRef b (Coord pt) = unsafeIndex b pt

boardPut :: a -> Board a -> Coord -> Board a
boardPut a b (Coord pt) = V.update b (V.singleton (pt, Point a))

boardFillwithChain :: a -> Board a -> Chain -> Board a
boardFillwithChain a b ch =
 b // zip (map toInt $ S.toList $ _chainCoords ch) (repeat $ Point a)

boardRemove :: Board a -> Coord -> Board a
boardRemove b (Coord pt) = V.update b (V.singleton (pt, Empty))

-- |
-- >>> aroundOf $ coord (2,3)
-- [coord (1,3),coord (3,3),coord (2,2),coord (2,4)]

aroundOf :: Coord -> [Coord]
aroundOf (Coord pt) = map Coord [pt-(boardSize+2), pt+(boardSize+2), pt-1, pt+1]

getChain :: GameStatus -> Coord -> Chain
getChain st pt = case boardRef (_chains st) pt of
                   (Point ch) -> ch
                   _ -> mempty

filterNeighbor :: Eq a => Board a -> Point a -> Coord -> [Coord]
filterNeighbor b p pt = filter ((==p).boardRef b) $ aroundOf pt

numOfLiberties :: Chain -> Int
numOfLiberties = S.size . _chainLiberties

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
                   any (>1) (map (numOfLiberties . getChain st) neighborSame)
      canKillOpponet =
          any (==1) $ map (numOfLiberties . getChain st) neighborOpponent

updateChain :: (Chain -> Chain) -> Coord -> Board Chain -> Board Chain
updateChain f pt chMap =
    case boardRef chMap pt of
      Point ch -> let ch' = f ch in boardFillwithChain ch' chMap ch'
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
          boardFillwithChain ch' cm ch

updateChainMap :: Coord -> [Chain] ->Chain -> Set Coord ->
                  Board Chain -> Board Chain
updateChainMap pt cs ch omitted chMap =
    removeChainFromMap omitted $
    removeLiberty pt cs $ boardFillwithChain ch chMap ch

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
          partition ((==1).numOfLiberties) $ map (getChain st) neighborOpponent
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
