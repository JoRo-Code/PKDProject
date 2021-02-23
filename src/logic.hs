module Logic where

import Graphics.Gloss.Interface.Pure.Game
import Data.Array

import Game
import Rendering

import Debug.Trace

validCoordinates :: CellCoord -> Bool
validCoordinates  = inRange boardIndex
                    where boardIndex = ((0, 0), (n - 1, n - 1)) 

endCoordinates :: CellCoord -> ShipSize -> Direction -> CellCoord
endCoordinates (c, r) s Horizontal = (c + s - 1, r)
endCoordinates (c, r) s Vertical = (c, r - s + 1)

getCell :: Board -> CellCoord -> Cell
getCell b c = b ! c

-- PRE: coordinates are in range, cell is NotChecked
-- Changes the state of a cell to checked
checkCell :: Board -> CellCoord -> Board
checkCell b (c, r) = case getCell b (c, r) of
                      Empty NotChecked -> b // [((c, r), Empty Checked)]   
                      Ship NotChecked  -> b // [((c, r), Ship Checked)]
                      _ -> b

-- PRE: coordinates are in range
-- returns the state of the cell at coordinates
getState :: Board -> CellCoord -> SquareState
getState b c =  case getCell b c of
                     Empty s -> s
                     Ship s -> s

-- returns True if a cell is Checked, else False
isChecked :: Board -> CellCoord -> Bool
isChecked b coord = s == Checked
        where s = getState b coord

playerShoot :: Game -> CellCoord -> Game
playerShoot game coord | validCoordinates coord && not (isChecked (gameBoardAI game) coord)
                        = game {gameBoardAI = checkCell (gameBoardAI game) coord, 
                                --gameStage = Shooting AI,
                                gameBoardUser = fst $ aiShoot (gameBoardUser game, stackAI game),
                                stackAI = snd $ aiShoot (gameBoardUser game, stackAI game)
                                } 
                       | otherwise = game

---------------------------- Placing ship ----------------------------
placeShip :: Game -> CellCoord -> ShipSize -> Direction -> Game
placeShip game _ 0 _= game
placeShip game coord s d | validShipPlacement (gameBoardUser game) coord s d  = game {gameBoardUser = placeShipAux (gameBoardUser game) coord s d, shipsUser = tail $ shipsUser game}
                         | otherwise = game

placeShipAux :: Board -> CellCoord -> ShipSize -> Direction -> Board
placeShipAux b _ 0 _= b
placeShipAux b (c, r) s d = placeShipAux (b // [((c, r), Ship NotChecked)]) (offset (c, r) d) (s - 1) d

validShipPlacement :: Board ->  CellCoord -> ShipSize -> Direction -> Bool
validShipPlacement b (c, r) s d = validCoordinates (endCoordinates (c, r) s d) -- (-1) because the ship part on (r, c) is included
                                  && validCoordinates (c, r)  
                                  && noCollision b (c, r) s d

-- cheeck if the ship the user want to place will collide with any existing ships
noCollision :: Board -> CellCoord -> ShipSize -> Direction -> Bool
noCollision b _ 0 _ = True
noCollision b (c, r) s d | b ! (c, r) == Ship NotChecked = False
                         | otherwise = noCollision b (offset (c, r) d) (s - 1) d
                           
offset :: CellCoord -> Direction -> (Col, Row)
offset (c, r) Vertical = (c, r - 1)
offset (c, r) Horizontal = (c + 1, r)

---------------------------- END Placing ship ------------------------
---------------------------- Moving Ship Picture ---------------------

mouseToCell :: ScreenCoord -> BoardPos -> CellCoord
mouseToCell (x, y) boardPos@((x1,y1),(x2,y2)) =  (floor ((x - x1 + boardWidth + screenDivider * 0.5) / cellWidth), floor ((y - y1 + boardHeight  * 0.5) / cellHeight ))
                                                                       
moveShip :: Game -> SpecialKey -> Game
moveShip game keyDir 
                     | validCoordinates (endCoordinates newCoord s d)
                       && validCoordinates newCoord = game {shipsUser = (newCoord, d, s) : tail (shipsUser game)}
                     | otherwise = game
                      where ((c, r), d, s) = head $ shipsUser game
                            newCoord =  case keyDir of
                                        KeyLeft  -> (c - 1, r)
                                        KeyRight -> (c + 1, r)  
                                        KeyUp    -> (c, r + 1)
                                        KeyDown  -> (c, r - 1)
                                        _        -> (c, r)

rotateShip :: Game -> Game
rotateShip game 
                | validCoordinates $ endCoordinates coord s newDirection =  game {shipsUser = (coord, newDirection, s) : tail (shipsUser game)}
                | otherwise = game
                  where (coord, d, s) = head $ shipsUser game
                        newDirection = case d of
                                      Horizontal -> Vertical
                                      Vertical   -> Horizontal

confirmShip :: Game -> Game
confirmShip game | validShipPlacement (gameBoardUser game) coord s d = (placeShip game coord s d) {gameStage = newGameStage}
                 | otherwise = game
                   where (coord, d, s) = head ships
                         ships = shipsUser game
                         newGameStage = if null $ tail ships then Shooting User else Placing User


---------------------------- END Moving Ship Picture -----------------


eventHandler :: Event -> Game -> Game
eventHandler (EventKey (SpecialKey KeyEnter) Down _ _) game  = 
     case gameStage game of 
         Placing User -> trace ("Enter: " ++ show (gameStage game)) confirmShip game
         _            -> trace ("Enter: " ++ show (gameStage game)) game
         where 
             ships = shipsUser game
eventHandler (EventKey (SpecialKey key) Down _ _) game       = 
    case gameStage game of 
        Placing User -> trace ("Arrow: " ++ show (gameStage game)) moveShip game key
        _            -> trace ("Arrow: " ++ show (gameStage game)) game
eventHandler (EventKey (Char 'r') Down _ _) game             = 
    case gameStage game of 
         Placing User -> rotateShip game
         _            -> game
eventHandler (EventKey (MouseButton LeftButton) Up _ mousePos) game = 
    case gameStage game of
         Shooting User -> playerShoot game $ mouseToCell mousePos boardAIPos -- should change gamestage to shooting AI
         --Placing User  -> placeShip game (mouseToCell mousePos boardUserPos) 7 Vertical-- change to a function that places ships instead
         _ -> game
eventHandler _ game = game 


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
  | aiHunt (head s) = (checkCell b (fst $ head s),cohesiveCells b (head s) ++ tail s)
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

--tester :: (Array (Int, Int) Cell, [a])
tester = (array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Ship NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Ship NotChecked),((2,0),Ship NotChecked),((2,1),Ship NotChecked),((2,2),Empty NotChecked)],[])