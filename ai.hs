{-# LANGUAGE LambdaCase #-} 

module AI where

------------------- Imports -------------------
import Data.Array
import System.IO
------------------- Imports -------------------

data Cell = Empty SquareState | Ship SquareState deriving (Show, Eq)     
data SquareState = Checked | NotChecked deriving (Show, Eq)         

type Board = Array (Row, Col) Cell                  
type Row = Int                                           
type Col = Int                                           
type Coordinates = (Row, Col)

type BoardSize = Int

type ShootList = [(Coordinates,Cell)]
type Stack = [(Coordinates,Cell)]

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

-- returns boardsize from given board
getBoardSize :: Board -> BoardSize
getBoardSize b =  round . sqrt . fromIntegral $ length b


--------------------- AI --------------------------------

-- Creates a ShootList from a row in a board
aiShootListRow :: Board -> Row -> ShootList
aiShootListRow board r = aiShootListRowAux board (r,0)
  where aiShootListRowAux :: Board -> Coordinates -> ShootList
        aiShootListRowAux board (r,c) | c == getBoardSize board = []
                                      | otherwise = ((r,c),getCell board (r,c)) : aiShootListRowAux board (r,c+1)

-- Creates a ShootList from all rows in a board
aiShootList :: Board -> ShootList
aiShootList board = aiShootListAux board 0
  where aiShootListAux :: Board -> Row -> ShootList
        aiShootListAux board r | r == getBoardSize board = []
                               | otherwise = aiShootListRow board r ++ aiShootListAux board (r+1)

-- Sorts a shootlist so AI prioritises cells that are not next to each other
aiPrio :: ShootList -> ShootList
aiPrio l = aiPrioAux l []
  where aiPrioAux :: ShootList -> ShootList -> ShootList
        aiPrioAux [] acc = acc
        aiPrioAux (x:xs) acc 
          | even $ getRow x = if  odd $ getCol x then aiPrioAux xs ([x] ++ acc) else aiPrioAux xs (acc ++ [x])
          | otherwise       = if even $ getCol x then aiPrioAux xs ([x] ++ acc) else aiPrioAux xs (acc ++ [x])

-- Returns column of element in a ShootList
getCol :: (Coordinates,Cell) -> Col
getCol ((_,b),_) = b

-- Returns row of element in a ShootList
getRow :: (Coordinates,Cell) -> Row
getRow ((a,_),_) = a

-- Returns True if a Cell contains an unchecked Ship, otherwise False
aiHunt :: (Coordinates,Cell) -> Bool
aiHunt ((_,_),Ship NotChecked) = True
aiHunt ((_,_),_) = False

-- Checks first cell in stack
aiShoot :: (Board,Stack) -> (Board,Stack)
aiShoot (b,s) = aiShootAux (b,s) (filterShootList (aiPrio (aiShootList b)) s)

-- Checks first cell in stack
--PRE: ShootList is not empty
aiShootAux :: (Board,Stack) -> ShootList -> (Board,Stack)
aiShootAux (b,[]) l = aiShootAux (b,[head l]) (tail l) -- Adds the head of ShootList to Stack if Stack is empty
aiShootAux (b,s) l 
  | aiHunt (head s) = (checkCell b (fst $ head s),cohesiveCells b (head s) ++ tail s)
  | otherwise = (checkCell b (fst $ head s),tail s)


-- Removes all checked cells and cells that are in Stack from ShootList
filterShootList :: ShootList -> Stack -> ShootList
filterShootList [] _ = []
filterShootList (x:xs) stack = filterShootListAux x stack ++ filterShootList xs stack
  where filterShootListAux ((_,_),Empty Checked) _ = []
        filterShootListAux ((_,_),Ship Checked) _ = []
        filterShootListAux ((r,c),cell) stack | notElem ((r,c),cell) stack = [((r,c),cell)]
                                              | otherwise = []

-- Returns a Stack of the cohesive cells to a cell in a row
cohesiveCellsRow :: Board -> (Coordinates,Cell) -> Stack
cohesiveCellsRow b ((r,c),x) 
  | validCoordinates (getBoardSize b) (r,c-1) && validCoordinates (getBoardSize b) (r,c+1) = [((r,c-1),getCell b (r,c-1)), ((r,c+1),getCell b (r,c+1))]
  | validCoordinates (getBoardSize b) (r,c-1) = [((r,c-1),getCell b (r,c-1))]
  | validCoordinates (getBoardSize b) (r,c+1) = [((r,c+1),getCell b (r,c+1))]
  | otherwise = []

-- Returns a Stack of all cohesive cells to a cell
cohesiveCells :: Board -> (Coordinates,Cell) -> Stack
cohesiveCells b ((r,c),x) 
  | validCoordinates (getBoardSize b) (r-1,c) && validCoordinates (getBoardSize b) (r+1,c) = [((r-1,c),getCell b (r-1,c)), ((r+1,c),getCell b (r+1,c))] ++ cohesiveCellsRow b ((r,c),x)
  | validCoordinates (getBoardSize b) (r-1,c) = [((r-1,c),getCell b (r-1,c))] ++ cohesiveCellsRow b ((r,c),x)
  | validCoordinates (getBoardSize b) (r+1,c) = [((r+1,c),getCell b (r+1,c))] ++ cohesiveCellsRow b ((r,c),x)
  | otherwise = cohesiveCellsRow b ((r,c),x)

tester :: (Array (Int, Int) Cell, [a])
tester = (array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Ship NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Ship NotChecked),((2,0),Ship NotChecked),((2,1),Ship NotChecked),((2,2),Empty NotChecked)],[])