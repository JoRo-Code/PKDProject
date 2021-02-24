{-# LANGUAGE LambdaCase #-} 
module Logic where

import Shuffle
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Data.Array

import Game
import Rendering

import Data.List
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
                        = game {gameBoardAI = shotAIboard, 
                                gameBoardUser = shotUserBoard,
                                stackAI = updatedAIstack,
                                winner = foo shotUserBoard shotAIboard
                                } 
                       | otherwise = game
                       where shotAIboard = checkCell (gameBoardAI game) coord
                             winCheck = isWinner shotUserBoard
                             (shotUserBoard, updatedAIstack) = aiShoot (gameBoardUser game, stackAI game) (gen game)

foo :: Board -> Board -> Maybe Player
foo boardUser boardAI = case (isWinner boardUser, isWinner boardAI) of
                             (True, True)  -> Just User
                             (True, False) -> Just AI
                             (False, True) -> Just User
                             (_, _)        -> Nothing

isWinner :: Board -> Bool
isWinner b = not $ any (\cell -> cell == Ship NotChecked) b


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
                                  && followPlacementRules b (c,r) s d

-- Make sure that no ships can be placed within the surrounding cells of another ship
followPlacementRules ::  Board -> CellCoord -> ShipSize -> Direction -> Bool
followPlacementRules b coord s d = all (\coord -> not (validCoordinates coord) || b ! coord /= Ship NotChecked) (surroundingCells b coord s d)

surroundingCells :: Board -> CellCoord -> ShipSize -> Direction -> [CellCoord]
surroundingCells b (c,r) s Horizontal =  [(c, r) | r <- [r-1..r+1], c <- [c..c+s-1]] ++ [(c-1, r)] ++ [(c+s,r)]
surroundingCells b (c,r) s Vertical   =  [(c, r) | c <- [c-1..c+1], r <- [r-s+1..r]] ++ [(c, r + 1)] ++ [(c, r +s)]                                                   
 
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
                | validCoordinates $ endCoordinates coord s newDirection =  game {shipsUser = (coord, newDirection, s) : tail ships}
                | otherwise = game
                  where (coord, d, s) = head ships
                        ships = shipsUser game
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
         Placing User -> confirmShip game
         _            -> game

eventHandler (EventKey (SpecialKey key) Down _ _) game       = 
    case gameStage game of 
        Placing User -> trace ("Current gen: " ++ show (gen game)) moveShip game key
        _            -> game

eventHandler (EventKey (Char 'r') Down _ _) game             = 
    case gameStage game of 
         Placing User -> rotateShip game
         _            -> game

eventHandler (EventKey (MouseButton LeftButton) Up _ mousePos) game =

    case (winner game, gameStage game) of
         (Nothing, Shooting User) -> playerShoot game $ mouseToCell mousePos boardAIPos -- should change gamestage to shooting AI
         (_, Shooting User) -> initGame
         _ -> game
eventHandler _ game = game 


--------------------- AI --------------------------------

-- Returns column of element in a ShootList
getCol :: (CellCoord,Cell) -> Col
getCol ((a,_),_) = a

-- Returns row of element in a ShootList
getRow :: (CellCoord,Cell) -> Row
getRow ((_,b),_) = b

-- Creates a ShootList from all Cols in a board
aiShootList :: Board -> ShootList
aiShootList board = [((c,r), board ! (c,r)) | c <- [0..n-1], r <- [0..n-1]]

-- Creates a ShootList of all cells with state NotChecked
filterShootList :: Board -> StdGen -> ShootList
filterShootList b gen = removeChecked (aiPrio (fst $ shuffle (aiShootList b) gen ) [])

-- Sorts a shootlist so AI prioritises cells that are not next to each other
aiPrio :: ShootList -> ShootList -> ShootList
aiPrio [] acc = acc
aiPrio (x:xs) acc 
  | even $ getCol x = if  odd $ getRow x then aiPrio xs ([x] ++ acc) else aiPrio xs (acc ++ [x])
  | otherwise       = if even $ getRow x then aiPrio xs ([x] ++ acc) else aiPrio xs (acc ++ [x])

aiPrio' :: ShootList -> ShootList
aiPrio' s = foldl combos [] s

combos :: ShootList -> (CellCoord,Cell) -> ShootList
combos acc e@((c, r), cell) = case (even c, even r) of
                              (True, False)  -> e : acc
                              (True, True)   -> acc ++ [e]
                              (False, True)  -> e : acc
                              (False, False) -> acc ++ [e]
             


-- Removes already checked cells from Stack or ShootList
removeChecked :: [(CellCoord, Cell)] -> [(CellCoord, Cell)]
removeChecked s = filter (\(coord, cell) -> cell == Empty NotChecked || cell == Ship NotChecked) s

-- Updates Stack with the head from ShootList if Stack is empty
updateStack :: Stack -> ShootList -> Stack
updateStack [] (x:xs) = [x]
updateStack s _ = s

-- Returns a Stack of the cohesive cells to a cell in a row
cohesiveCells :: Board -> (CellCoord,Cell) -> Stack
cohesiveCells b ((c,r),x) = map (\coord -> (coord, b ! coord)) (filter validCoordinates [(c,r+1), (c,r-1), (c-1,r), (c+1,r)])

-- Returns True if a Cell contains an unchecked Ship, otherwise False
aiHunt :: (CellCoord,Cell) -> Bool
aiHunt ((_,_),Ship NotChecked) = True
aiHunt ((_,_),_) = False

-- Checks first cell in stack
--PRE: ShootList is not empty
aiShootAux :: (Board,Stack) -> ShootList -> (Board,Stack)
aiShootAux (b,s) l 
  | aiHunt (head s) = (checkCell b (fst $ head s),removeChecked $ nub (cohesiveCells b (head s) ++ tail s))
  | otherwise = (checkCell b (fst $ head s),tail s)

-- Checks first cell in stack
aiShoot :: (Board,Stack) -> StdGen -> (Board,Stack)
aiShoot (b,s) gen = aiShootAux (b,removeChecked $ updateStack s (filterShootList b gen)) (filterShootList b gen)