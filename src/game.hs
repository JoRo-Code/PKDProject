module Game where

import Data.Array

data SquareState = Checked | NotChecked deriving (Show, Eq)
data Cell = Empty SquareState | Ship SquareState deriving (Show, Eq)



type Row = Int
type Col = Int
type CellCoordinates = (Row, Col)

type Board = Array (Row, Col) Cell

type BoardSize = Int

data Game = Game { gameBoard :: Board } deriving (Show, Eq)

-- Create a new board, a 2d array, where all cells are empty notchecked initially.                                                         
initBoard :: BoardSize -> Board        
initBoard s = array boardIndex $ zip (range boardIndex) (repeat $ Empty NotChecked)
             where boardIndex = ((0, 0), (s - 1, s - 1)) 

initialGame :: Game
initialGame = Game { gameBoard = initBoard 3 }
