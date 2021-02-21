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

data Game = Game { gameBoardUser :: Board , 
                   gameBoardAI   :: Board,
                   gameStage     :: GameStage
                 } deriving (Show, Eq)

n :: BoardSize
n = 10

-- Create a new board, a 2d array, where all cells are empty notchecked initially.                                                         
initBoard :: BoardSize -> Board        
initBoard s = array boardIndex $ zip (range boardIndex) (repeat $ Empty NotChecked)
             where boardIndex = ((0, 0), (s - 1, s - 1)) 

initGame :: Game
initGame = Game { gameBoardUser = initBoard n,
                  gameBoardAI   = initBoard n,
                  gameStage     = Placing User
                }
