module HomMad.Goban where

import Data.Monoid
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Foldable as F
import Data.List (foldl')

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

toIdx :: Coord -> Int
toIdx (row, col) = row*boardSize + col

type ChainId = Int

data GameStatus = GameStatus {
      _board :: Board Color    -- ^Board status
    , _turn :: Color           -- ^Turn
    , _ko :: Maybe Coord       -- ^Ko
    , _chainMap :: Board ChainId
    , _chains :: IntMap Chain
    } deriving (Show, Eq)

data Chain = Chain {
      _chainId :: ChainId
    , _chainSize :: Int
    , _chainCoords :: Set Coord -- ^Coords of the chain stones
    } deriving (Show, Eq, Ord)

instance Monoid Chain where
    mempty = Chain (-1) 0 S.empty
    mappend c1 c2
        | c1 == c2 = c1
        | c1 == mempty = c2
        | c2 == mempty = c1
        | otherwise = Chain newId newSize coods
      where
        coods = _chainCoords c1 `S.union` _chainCoords c2
        newSize = _chainSize c1 + _chainSize c2
        newId = toIdx $ S.findMax coods

emptyBoard :: Board a
emptyBoard = IM.empty

initGame :: GameStatus
initGame = GameStatus emptyBoard Black Nothing emptyBoard IM.empty

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
    | otherwise = maybe Empty Point $ IM.lookup (toIdx pt) b

boardPut :: a -> Board a -> Coord -> Board a
boardPut a b pt = IM.insert (toIdx pt) a b

boardRemove :: Board a -> Coord -> Board a
boardRemove b pt = IM.delete (toIdx pt) b

-- |
-- >>> aroundOf (0,0)
-- [(-1,0),(1,0),(0,-1),(0,1)]

aroundOf :: Coord -> [Coord]
aroundOf (row, col) = [(row-1, col), (row+1, col), (row, col-1), (row, col+1)]

getChain :: GameStatus -> Coord -> Chain
getChain st pt = maybe mempty id $
                 case boardRef (_chainMap st) pt of
                   (Point chId) -> IM.lookup chId $ _chains st
                   _ -> Nothing

liberties :: Board Color -> Chain -> Set Coord
liberties b ch =
    F.foldMap (S.fromList . filterNeighbor b Empty) $ _chainCoords ch

filterNeighbor :: Eq a => Board a -> Point a -> Coord -> [Coord]
filterNeighbor b p coord = filter ((==p).boardRef b) $ aroundOf coord

isLastLiberty :: Board Color -> Coord -> Chain -> Bool
isLastLiberty b pt = (==S.singleton pt) . liberties b

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
                   any (not.isLastLiberty b pt) (map (getChain st) neighborSame)
      canKillOpponet =
          any (isLastLiberty b pt) $ map (getChain st) neighborOpponent

updateChainMap :: Chain -> Set Coord -> Board ChainId -> Board ChainId
updateChainMap Chain{_chainId=chId, _chainCoords=addend} omitted chMap =
    S.foldl' boardRemove (S.foldl' (boardPut chId) chMap addend) omitted

putStone :: GameStatus -> Coord -> GameStatus
putStone st@(GameStatus b t _ cm cs) pt =
    GameStatus newBoard (opponent t) ko newChainMap newChains
    where
      neighborSame = filterNeighbor b (Point t) pt
      neighborOpponent = filterNeighbor b (Point $ opponent t) pt
      chainSingleton = Chain (toIdx pt) 1 (S.singleton pt)
      chainConnected =
          mconcat $ chainSingleton : map (getChain st) neighborSame
      chainCaptured =
          filter (isLastLiberty b pt) $ map (getChain st) neighborOpponent
      captured = F.foldMap _chainCoords chainCaptured
      newBoard' = boardPut t b pt
      newBoard = S.foldl' boardRemove newBoard' captured
      ko = if S.size captured == 1 &&
              _chainSize chainConnected == 1 &&
              (S.null $ liberties b chainConnected)
           then Just $ S.toList captured !! 0
           else Nothing
      newChainMap = updateChainMap chainConnected captured cm
      newChains = foldl' (flip IM.delete)
                  (IM.insert (_chainId chainConnected) chainConnected cs) $
                  map _chainId chainCaptured

pass :: GameStatus -> GameStatus
pass st@GameStatus{_turn=t} = st{_turn=opponent t}
