module Game where

import Data.Array

data SquareState  = Checked           | NotChecked                 deriving (Show, Eq)
data Cell         = Empty SquareState | Ship SquareState           deriving (Show, Eq)
data Player       = User              | AI                         deriving (Show, Eq)
data GameStage    = Placing Player    | Shooting Player            deriving (Show, Eq)
data Direction    = Horizontal        | Vertical                   deriving (Show, Eq)  

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
type Ships = [(CellCoord, Direction, ShipSize)]

data Game = Game { gameBoardUser :: Board , 
                   gameBoardAI   :: Board,
                   gameStage     :: GameStage,
                   shipsUser     :: Ships,
                   stackAI       :: Stack,
                   winner        :: Maybe Player
                  
                 } deriving (Show, Eq)

n :: BoardSize
n = 10

-- Create a new board, a 2d array, where all cells are empty notchecked initially.                                                         
initBoard :: Board        
initBoard = array boardIndex $ zip (range boardIndex) (repeat $ Empty NotChecked)
            where boardIndex = ((0, 0), (n - 1, n - 1)) 


initShips :: Ships
initShips = [((0,4), Vertical, 5),((0,4), Vertical, 5),((0,4), Vertical, 5),((0,4), Vertical, 5),((0,4), Vertical, 5)]--[((0, 0), Horizontal, 5), ((0, 3), Vertical, 4), ((0, 0), Horizontal, 3), ((0, 1), Vertical, 2)]

initGame :: Game
initGame = Game { gameBoardUser = initBoard,--array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Ship NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Ship NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked)],
                  gameBoardAI   = initBoard,
                  gameStage     = Placing User,
                  shipsUser     = initShips,
                  stackAI       = [],
                  winner        = Nothing
                }
