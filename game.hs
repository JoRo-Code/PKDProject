{-# LANGUAGE LambdaCase #-} 
import Data.Array
                                     
                                                         
data Cell = Empty State | Ship State deriving (Show, Eq)     
data State = Checked | NotChecked deriving (Show, Eq)        
data Direction = Horizontal | Vertical deriving (Show, Eq)   

type Board = Array (Int, Int) Cell                       
type Row = Int                                           
type Col = Int                                           
type Coordinates = (Row, Col)

type Size = Int
                                                         
initBoard :: Size -> Array (Int, Int) Cell                
initBoard s = array boardIndex $ zip (range boardIndex) (repeat $ Empty NotChecked)
             where boardIndex = ((0, 0), (s - 1, s - 1))  

-- PRE: coordinates are in range
-- Changes the state of a cell to checked
updateCell :: Board -> Coordinates -> Board
updateCell b (r, c) = case getCell b (r, c) of
                      Empty NotChecked -> b // [((r,c), Empty Checked)]   
                      Ship NotChecked  -> b // [((r,c), Ship Checked)]

-- Give size of board, and coordinates
-- returns true if coordinates are in boardIndex, else false
validCoordinates :: Size -> Coordinates -> Bool
validCoordinates s (r, c) = inRange boardIndex (r, c)
                           where boardIndex = ((0, 0), (s - 1, s - 1))  

-- PRE: coordinates are in range
-- returns the cell at coordinates
getCell :: Board -> Coordinates -> Cell
getCell b c = b ! c

-- PRE: coordinates are in range
-- returns the state of the cell at coordinates
getState :: Board -> Coordinates -> State
getState b c =  case getCell b c of
                     Empty s -> s
                     Ship s -> s

-- PRE: cell is empty at coordinates
-- place down a shipcell at given coordinates
placeShipCell :: Board -> Coordinates -> Board
placeShipCell b (r, c) = b // [((r,c), Ship NotChecked)]   

-- PRE: coordinates are in range, ship placement wont go out of bounds
--      if direction == horizontal, the coordinates is the top part of the ship,
--      if vertical its the far most left part of the ship
-- examples: if size 3, coord c, direction vertical:      s (coord)
--                                                        s
--                                                        s

placeShip :: Board -> Coordinates -> Size -> Direction -> Board
placeShip b coords s d = case d of
                        Horizontal -> placeHorizontal b coords s
                        Vertical   -> placeVertical b coords s

{- 
  We need to implement following, somewhere along the way:
* Handle the case when coordinates are out of range (incl. when any part's coordinates are out of range)
* Handle the case when ny ship part crosses another ship part already placed, 
  thus rendering the placement invalid.
-}

placeHorizontal :: Board -> Coordinates -> Size -> Board
placeHorizontal b _ 0 = b
placeHorizontal b (r, c) s = placeHorizontal (b // [((r, c), Ship NotChecked)]) (r, c + 1) (s - 1)

placeVertical :: Board -> Coordinates -> Size -> Board
placeVertical b _ 0 = b
placeVertical b (r, c) s = placeVertical (b // [((r, c), Ship NotChecked)]) (r + 1, c) (s - 1)


-- Check if all ships are Checked
winner :: Board -> Bool
winner b = not(any (\cell -> cell == Ship NotChecked) b)

-- Board to list of cells
cellList :: Board -> [Cell]
cellList = elems 

-- Board to list of state
-- Used to print other players board, but without displaying ships, just where player have already checked.
stateList :: Board -> [State]
stateList b = map (\case (Empty s) -> s; (Ship s) -> s) (elems b)


b = initBoard 10


{- 

When player1 is playing their action manipulates player2's array. What's being displayed should just be the states (and in case of ship, display in other way)

Drawing when it's player1's turn:
Go through the array and draw a cross at every element that is checked. 
If is is checked and a ship, display it in another way

Shooting:

If input is valid, update player2's array and call the draw func


-}