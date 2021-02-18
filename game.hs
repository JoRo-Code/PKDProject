{-# LANGUAGE LambdaCase #-} 

module Game where

------------------- Imports -------------------
import Data.Array
import System.IO
------------------- Imports -------------------

data Cell = Empty SquareState | Ship SquareState deriving (Show, Eq)     
data SquareState = Checked | NotChecked deriving (Show, Eq)        
data Direction = Horizontal | Vertical deriving (Show, Eq)   

type Board = Array (Row, Col) Cell                  
type Row = Int                                           
type Col = Int                                           
type Coordinates = (Row, Col)

type BoardSize = Int
type ShipSize = Int

-- Create a new board, a 2d array, where all cells are empty notchecked initially.                                                         
initBoard :: BoardSize -> Board        
initBoard s = array boardIndex $ zip (range boardIndex) (repeat $ Empty NotChecked)
             where boardIndex = ((0, 0), (s - 1, s - 1))  

-- PRE: coordinates are in range, cell is NotChecked
-- Changes the state of a cell to checked
checkCell :: Board -> Coordinates -> Board
checkCell b (r, c) = case getCell b (r, c) of
                      Empty NotChecked -> b // [((r,c), Empty Checked)]   
                      Ship NotChecked  -> b // [((r,c), Ship Checked)]

-- Give size of board, and coordinates
-- returns true if coordinates are in boardIndex, else false
validCoordinates :: BoardSize -> Coordinates -> Bool
validCoordinates s = inRange boardIndex
                    where boardIndex = ((0, 0), (s - 1, s - 1))  

-- PRE: coordinates are in range
-- returns the cell at coordinates
getCell :: Board -> Coordinates -> Cell
getCell b c = b ! c

-- PRE: coordinates are in range
-- returns the state of the cell at coordinates
getState :: Board -> Coordinates -> SquareState
getState b c =  case getCell b c of
                     Empty s -> s
                     Ship s -> s

-- PRE: coordinates are in range, ship placement wont go out of bounds
placeShip :: Board -> Coordinates -> ShipSize -> Direction -> Board
placeShip b _ 0 _= b
placeShip b (r, c) s d = placeShip (b // [((r, c), Ship NotChecked)]) (r + rowOffset, c + colOffset) (s - 1) d
                        where (rowOffset, colOffset) = offset d

validShipPlacement :: Board -> Coordinates -> ShipSize -> Direction -> Bool
validShipPlacement b (r, c) s d = validCoordinates (getBoardSize b) (r + rowOffset * s - 1, c + colOffset * s - 1) -- (-1) because the ship part on (r, c) is included
                                && validCoordinates (getBoardSize b) (r, c) 
                                && noCollision b (r, c) s d
                                where (rowOffset, colOffset) = offset d

-- cheeck if the ship the user want to place will collide with any existing ships
noCollision :: Board -> Coordinates -> ShipSize -> Direction -> Bool
noCollision b _ 0 _ = True
noCollision b (r, c) s d | b ! (r, c) == Ship NotChecked = False
                         | otherwise = noCollision b (r + rowOffset, c + colOffset) (s - 1) d
                           where (rowOffset, colOffset) = offset d
-- calculate offset in row and col
offset :: Direction -> (Row, Col)
offset Vertical = (1, 0)
offset Horizontal = (0, 1)

-- returns boardsize from given board
getBoardSize :: Board -> BoardSize
getBoardSize b =  round . sqrt . fromIntegral $ length b

-- Should only be called after all ships have been placed
-- Check if all ships are Checked
winner :: Board -> Bool
winner b = not $ any (\cell -> cell == Ship NotChecked) b

-- Board to list of cells
cellList :: Board -> [Cell]
cellList = elems 

-- Board to list of state
-- Used to print other players board, but without displaying ships, just where player have already checked.
stateList :: Board -> [SquareState]
stateList b = map (\case (Empty s) -> s; (Ship s) -> s) (elems b)

b :: Board
b = initBoard 10

------------------- File -------------------

path :: FilePath 
path = "fileTest.txt"

arrayToFile :: Board -> IO()
arrayToFile b = do
     appendFile path $ show (elems b)

fileToArray :: IO ()
fileToArray = do
    handle  <- openFile path ReadMode
    content <- hGetContents handle
    putStr content
    hClose handle

------------------- File -------------------

{- 
When player1 is playing their action manipulates player2's array. What's being displayed should just be the states (and in case of ship, display in other way)

Drawing when it's player1's turn:
Go through the array and draw a cross at every element that is checked. 
If is is checked and a ship, display it in another way

Shooting:

If input is valid, update player2's array and call the draw func
-}