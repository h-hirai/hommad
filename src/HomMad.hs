module HomMad where

boardSize :: Int
boardSize = 9

-- |Status of a point on a board.
data Color
    = B -- ^Black
    | W -- ^White
    | E -- ^Empty
    | O -- ^Out Of Boad
    deriving (Show, Eq)

type Board = [[Color]]

type Point = (Int, Int)

data GameStatus
    = GameStatus {
        _board :: Board          -- ^Board status
      , _turn :: Color           -- ^Turn
      , _prisonersB :: Int       -- ^Captured by Black
      , _prisonersW :: Int       -- ^Captured by White
      , _ko :: Maybe Point       -- ^Ko
      } deriving (Show, Eq)

-- |
-- >>> boardRef [[E,B],[W,E]] (0,1)
-- B
-- >>> boardRef [[E,B],[W,E]] (1,1)
-- E
-- >>> boardRef [[E,B],[W,E]] (1,-1)
-- O
-- >>> boardRef [[E,B],[W,E]] (boardSize,1)
-- O
boardRef :: Board -> Point -> Color
boardRef b (row, col) | row < 0 = O
                      | row >= boardSize = O
                      | col < 0 = O
                      | col >= boardSize = O
                      | otherwise = b !! row !! col

boardPut :: Color -> Board -> Point -> Board
boardPut = undefined

canPut :: GameStatus -> Point -> Bool
canPut = undefined

putStone :: GameStatus -> Point -> GameStatus
putStone = undefined
