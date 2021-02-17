import Data.Array                                       
                                                         
data Cell = Empty State | Ship State deriving (Show)     
                                                         
data State = Checked | NotChecked deriving (Show)        
                                                         
type Board = Array (Int, Int) Cell                       
type Row = Int                                           
type Col = Int                                           
type Coordinates = (Row, Col)                            
                                                         
initBoard :: Int -> Array (Int, Int) Cell                
initBoard n = array boardIndex $ zip (range boardIndex) (repeat (Empty NotChecked))
            where boardIndex = ((0, 0), (n - 1, n - 1))  

-- PRE: coordinates are in range
-- Changes the state of a cell to checked
checked :: Board -> Coordinates -> Board
checked b (r, c) = case getCell b (r, c) of
                      Empty NotChecked -> b // [((r,c), Empty Checked)]   
                      Ship NotChecked  -> b // [((r,c), Ship Checked)]   

-- PRE: coordinates are in range
-- returns the cell at coordinates
getCell :: Board -> Coordinates -> Cell
getCell b (r, c) = b ! (r, c)

-- PRE: cell is empty at coordinates
-- place down a shipcell at given coordinates
placeShipCell :: Board -> Coordinates -> Board
placeShipCell b (r, c) = b // [((r,c), Ship NotChecked)]   


b = initBoard 10