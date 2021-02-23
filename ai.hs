{-# LANGUAGE LambdaCase #-} 

module AI where




------------------- Imports -------------------
import Data.Array
import System.IO

--import Data.Stack
------------------- Imports -------------------
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

type ShootList = [(CellCoord,Cell)]
type Stack = [(CellCoord,Cell)]


-- returns boardsize from given board

validCoordinates :: CellCoord -> Bool
validCoordinates  = inRange boardIndex
                    where boardIndex = ((0, 0), (n - 1, n - 1)) 

getCell :: Board -> CellCoord -> Cell
getCell b c = b ! c

checkCell :: Board -> CellCoord -> Board
checkCell b (c, r) = case getCell b (c, r) of
                      Empty NotChecked -> b // [((c, r), Empty Checked)]   
                      Ship NotChecked  -> b // [((c, r), Ship Checked)]
                      _ -> b

initBoard :: Board        
initBoard = array boardIndex $ zip (range boardIndex) (repeat $ Ship NotChecked)
             where boardIndex = ((0, 0), (n - 1, n - 1)) 

n :: BoardSize
n = 4
--------------------- AI --------------------------------

-- Creates a ShootList from a Col in a board
aiShootListCol :: Board -> Col -> ShootList
aiShootListCol board r = aiShootListColAux board (r,0)
  where aiShootListColAux :: Board -> CellCoord -> ShootList
        aiShootListColAux board (c,r) | r == n = []
                                      | otherwise = ((c,r),getCell board (c,r)) : aiShootListColAux board (c,r+1)

-- Creates a ShootList from all Cols in a board
aiShootList :: Board -> ShootList
aiShootList board = aiShootListAux board 0
  where aiShootListAux :: Board -> Col -> ShootList
        aiShootListAux board c | c == n = []
                               | otherwise = aiShootListCol board c ++ aiShootListAux board (c+1)

-- Sorts a shootlist so AI prioritises cells that are not next to each other
aiPrio :: ShootList -> ShootList
aiPrio l = aiPrioAux l []
  where aiPrioAux :: ShootList -> ShootList -> ShootList
        aiPrioAux [] acc = acc
        aiPrioAux (x:xs) acc 
          | even $ getCol x = if  odd $ getRow x then aiPrioAux xs ([x] ++ acc) else aiPrioAux xs (acc ++ [x])
          | otherwise       = if even $ getRow x then aiPrioAux xs ([x] ++ acc) else aiPrioAux xs (acc ++ [x])

-- Returns column of element in a ShootList
getCol :: (CellCoord,Cell) -> Col
getCol ((a,_),_) = a

-- Returns row of element in a ShootList
getRow :: (CellCoord,Cell) -> Row
getRow ((_,b),_) = b

-- Returns True if a Cell contains an unchecked Ship, otherwise False
aiHunt :: (CellCoord,Cell) -> Bool
aiHunt ((_,_),Ship NotChecked) = True
aiHunt ((_,_),_) = False

-- Checks first cell in stack
aiShoot :: (Board,Stack) -> (Board,Stack)
aiShoot (b,s) = aiShootAux (b,removeChecked $ updateStack s (filterShootList b)) (filterShootList b)

-- Checks first cell in stack
--PRE: ShootList is not empty
aiShootAux :: (Board,Stack) -> ShootList -> (Board,Stack)
aiShootAux (b,s) l 
  | aiHunt (head s) = (checkCell b (fst $ head s),removeChecked $ removeDuplicates (cohesiveCells b (head s) ++ tail s) [])
  | otherwise = (checkCell b (fst $ head s),tail s)

-- Creates a ShootList of all cells with state NotChecked
filterShootList :: Board -> ShootList
filterShootList b = removeChecked (aiPrio (aiShootList b))

-- Removes already checked cells from Stack or ShootList
removeChecked :: [(CellCoord,Cell)] -> [(CellCoord,Cell)]
removeChecked [] = []
removeChecked (x:xs) = removeCheckedAux x ++ removeChecked xs
  where removeCheckedAux :: (CellCoord,Cell) -> [(CellCoord,Cell)]
        removeCheckedAux ((_,_),Ship Checked) = []
        removeCheckedAux ((_,_),Empty Checked) = []
        removeCheckedAux x = [x]

-- Removes duplicte cells from Stack
removeDuplicates :: Stack -> Stack -> Stack
removeDuplicates [] ns = ns
removeDuplicates (x:xs) ns 
  | elem x ns = removeDuplicates xs ns
  | otherwise = removeDuplicates xs (ns ++ [x])


-- Updates Stack with the head from ShootList if Stack is empty
updateStack :: Stack -> ShootList -> Stack
updateStack [] (x:xs) = [x]
updateStack s _ = s

-- Returns a Stack of the cohesive cells to a cell in a row
cohesiveCellsRow :: Board -> (CellCoord,Cell) -> Stack
cohesiveCellsRow b ((c,r),x) 
  | validCoordinates (c,r-1) && validCoordinates (c,r+1) = [((c,r-1),getCell b (c,r-1)), ((c,r+1),getCell b (c,r+1))]
  | validCoordinates (c,r-1) = [((c,r-1),getCell b (c,r-1))]
  | validCoordinates (c,r+1) = [((c,r+1),getCell b (c,r+1))]
  | otherwise = []

-- Returns a Stack of all cohesive cells to a cell
cohesiveCells :: Board -> (CellCoord,Cell) -> Stack
cohesiveCells b ((c,r),x) 
  | validCoordinates (c-1,r) && validCoordinates (c+1,r) = [((c-1,r),getCell b (c-1,r)), ((c+1,r),getCell b (c+1,r))] ++ cohesiveCellsRow b ((c,r),x)
  | validCoordinates (c-1,r) = [((c-1,r),getCell b (c-1,r))] ++ cohesiveCellsRow b ((c,r),x)
  | validCoordinates (c+1,r) = [((c+1,r),getCell b (c+1,r))] ++ cohesiveCellsRow b ((c,r),x)
  | otherwise = cohesiveCellsRow b ((c,r),x)

tester :: (Array (Int, Int) Cell, [a])
tester = (array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Ship NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Ship NotChecked),((2,0),Ship NotChecked),((2,1),Ship NotChecked),((2,2),Empty NotChecked)],[])
tester2 :: (Array (Int, Int) Cell, [a])
tester2 = (array ((0,0),(2,2)) [((0,0),Ship NotChecked),((0,1),Ship NotChecked),((0,2),Ship NotChecked),((1,0),Ship NotChecked),((1,1),Ship NotChecked),((1,2),Ship NotChecked),((2,0),Ship NotChecked),((2,1),Ship NotChecked),((2,2),Ship NotChecked)],[])

--BUG: Duplicates appear in stack