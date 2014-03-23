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


boardRef :: Board -> Point -> Color
boardRef = undefined

boardPut :: Color -> Board -> Point -> Board
boardPut = undefined

canPut :: GameStatus -> Point -> Bool
canPut = undefined

putStone :: GameStatus -> Point -> GameStatus
putStone = undefined
