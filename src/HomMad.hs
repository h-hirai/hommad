module HomMad where

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


emptyBoard :: Board
emptyBoard = replicate boardSize $ replicate boardSize E

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

canPut :: GameStatus -> Point -> Bool
canPut GameStatus{_board=b} p | boardRef b p == E = True
                              | otherwise         = False

putStone :: GameStatus -> Point -> GameStatus
putStone st@GameStatus{_board=b, _turn=t} p =
    st{_board=boardPut t b p, _turn=next t}
    where next B = W
          next W = B
