module Game where

import Data.Array

data SquareState = Checked           | NotChecked                 deriving (Show, Eq)
data Cell        = Empty SquareState | Ship SquareState           deriving (Show, Eq)
data Player      = User              | AI                         deriving (Show, Eq)
data GameStage   = Placing Player    | Shooting Player            deriving (Show, Eq)
data Direction   = Horizontal        | Vertical                   deriving (Show, Eq)  

type Row         = Int
type Col         = Int
type CellCoord   = (Col, Row)
type ScreenCoord = (Float, Float)
type Board       = Array (Col, Row) Cell
type BoardSize   = Int
type ShipSize    = Int



-- AI-types
type ShootList = [(CellCoord,Cell)]
type Stack = [(CellCoord,Cell)]

{- cellCoord: starting coord of ship,
  Direction: continuing right or down,
  ShipSize: length of the ship
-}
type Ships = (CellCoord, Direction, ShipSize)

data Game = Game { gameBoardUser :: Board , 
                   gameBoardAI   :: Board,
                   gameStage     :: GameStage,
                   shipsUser     :: Ships,
                   stackAI       :: Stack
                  
                 } deriving (Show, Eq)

n :: BoardSize
n = 5

-- Create a new board, a 2d array, where all cells are empty notchecked initially.                                                         
initBoard :: BoardSize -> Board        
initBoard s = array boardIndex $ zip (range boardIndex) (repeat $ Ship NotChecked)
             where boardIndex = ((0, 0), (s - 1, s - 1)) 


initShip :: Ships
initShip = ((4,4), Vertical, 5)

initGame :: Game
initGame = Game { gameBoardUser = initBoard n,--array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Ship NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Ship NotChecked),((2,0),Ship NotChecked),((2,1),Ship NotChecked),((2,2),Empty NotChecked)],
                  gameBoardAI   = initBoard n,
                  gameStage     = Shooting User,
                  shipsUser     = initShip,
                  stackAI       = []
                }
