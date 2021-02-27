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


---------------------------- Placing ship ----------------------------

offset :: CellCoord -> Direction -> (Col, Row)
offset (c, r) Vertical = (c, r - 1)
offset (c, r) Horizontal = (c + 1, r)

placeShipAux :: Board -> CellCoord -> ShipSize -> Direction -> Board
placeShipAux b _ 0 _= b
placeShipAux b (c, r) s d = placeShipAux (b // [((c, r), Ship NotChecked)]) (offset (c, r) d) (s - 1) d

surroundingCells :: Board -> CellCoord -> ShipSize -> Direction -> [CellCoord]
surroundingCells b (c,r) s Horizontal =  [(c, r) | r <- [r-1..r+1], c <- [c..c+s-1]] ++ [(c-1, r)] ++ [(c+s,r)]
surroundingCells b (c,r) s Vertical   =  [(c, r) | c <- [c-1..c+1], r <- [r-s+1..r]] ++ [(c, r + 1)] ++ [(c, r -s)]                                                   


validCoordinates :: CellCoord -> Bool
validCoordinates  = inRange boardIndex
                    where boardIndex = ((0, 0), (n - 1, n - 1)) 

-- Make sure that no ships can be placed within the surrounding cells of another ship
followPlacementRules ::  Board -> CellCoord -> ShipSize -> Direction -> Bool
followPlacementRules b coord s d = all (\coord -> not (validCoordinates coord) || b ! coord /= Ship NotChecked) (surroundingCells b coord s d)

endCoordinates :: CellCoord -> ShipSize -> Direction -> CellCoord
endCoordinates (c, r) s Horizontal = (c + s - 1, r)
endCoordinates (c, r) s Vertical = (c, r - s + 1)

validShipPlacement :: Board ->  CellCoord -> ShipSize -> Direction -> Bool
validShipPlacement b (c, r) s d = validCoordinates (endCoordinates (c, r) s d) -- (-1) because the ship part on (r, c) is included
                                  && validCoordinates (c, r)  
                                  && followPlacementRules b (c,r) s d

placeShip :: Game -> CellCoord -> ShipSize -> Direction -> Game
placeShip game _ 0 _= game
placeShip game coord s d | validShipPlacement (gameBoardUser game) coord s d  = game {gameBoardUser = placeShipAux (gameBoardUser game) coord s d, shipsUser = tail $ shipsUser game}
                         | otherwise = game




---------------------------- Moving Ship Picture ---------------------

{- mouseToCell mousePos boardPos
    calculates current cell from mouseposition
    RETURNS: the corresponding cellCoordinates for mousePos on a board with boardPos

-}
mouseToCell :: ScreenCoord -> BoardPos -> CellCoord
mouseToCell (x, y) boardPos@((x1,y1),(x2,y2)) =  (floor ((x - x1 + boardWidth + screenDivider * 0.5) / cellWidth), floor ((y - y1 + boardHeight  * 0.5) / cellHeight ))


{- moveShip game key
    updates the first placing ship's cellCoordinates according to key input
    RETURNS: If valid position then head of shipsUser game with new coordinates according to key, else game.

-}
moveShip :: Game -> SpecialKey -> Game
moveShip game keyDir 
                     | validCoordinates (endCoordinates newCoord s d)
                       && validCoordinates newCoord = game {shipsUser = (newCoord, d, s) : rest}
                     | otherwise = game
                      where (((c, r), d, s):rest) = shipsUser game
                            newCoord =  case keyDir of
                                        KeyLeft  -> (c - 1, r)
                                        KeyRight -> (c + 1, r)  
                                        KeyUp    -> (c, r + 1)
                                        KeyDown  -> (c, r - 1)
                                        _        -> (c, r)

{- rotateShip game 
    changes the first placing ship's direction
    RETURNS: If opposite direction is valid then head of shipsUser game with opposite direction, else game.

-}
rotateShip :: Game -> Game
rotateShip game 
                | validCoordinates $ endCoordinates coord s newDirection =  game {shipsUser = (coord, newDirection, s) : tail ships}
                | otherwise = game
                  where (coord, d, s) = head ships
                        ships = shipsUser game
                        newDirection = case d of
                                      Horizontal -> Vertical
                                      Vertical   -> Horizontal
{- confirmShip game
    puts placing ship on board and changes gameStage if all ships have been placed
    RETURNS: -------
-}
confirmShip :: Game -> Game
confirmShip game | validShipPlacement (gameBoardUser game) coord s d = (placeShip game coord s d) {gameStage = newGameStage}
                 | otherwise = game
                   where (coord, d, s) = head ships
                         ships = shipsUser game
                         newGameStage = if null $ tail ships then Shooting User else Placing User





---------------------------- Shooting Ship ------------------------

{- getCell board coord
    extracts board contents on a specific column and row
    PRE: coord in board
    RETURNS: content of coord in board

-}
getCell :: Board -> CellCoord -> Cell
getCell b c = b ! c


{- checkCell board coord
    Changes the state of a cell to checked
    PRE: coord in board
    RETURNS: if coord == unchecked then board with checked coord, else board.

-}
checkCell :: Board -> CellCoord -> Board
checkCell b (c, r) = case getCell b (c, r) of
                      Empty NotChecked -> b // [((c, r), Empty Checked)]   
                      Ship NotChecked  -> b // [((c, r), Ship Checked)]
                      _ -> b


{- getState board coord
    gets the state of a cell
    PRE: coord in board
    RETURNS: state of coord in board
-}
getState :: Board -> CellCoord -> SquareState
getState b c =  case getCell b c of
                     Empty s -> s
                     Ship s -> s

hitShip :: Board -> CellCoord -> Bool 
hitShip b coord = validCoordinates coord && b ! coord == Ship NotChecked

isWithinBoard :: BoardPos -> ScreenCoord -> Bool
isWithinBoard ((x1,y1),(x2,y2)) (x,y) = xNew >= x1 && xNew <= x2 && yNew >= y1 && yNew <= y2
                                      where (xNew, yNew) = (x + 0.5 * screenWidth, y + 0.5*screenHeight)

-- returns True if a cell is Checked, else False

{- isChecked board coord
    evaluates if coord is checked
    PRE: coord in board
    RETURNS: True if coord == checked, else False.
-}
isChecked :: Board -> CellCoord -> Bool
isChecked b coord = s == Checked
        where s = getState b coord



{- allShipsChecked board 
    checks if board has won
    RETURNS: if all ships are checked on board
-}
allShipsChecked :: Board -> Bool
allShipsChecked b = not $ any (\cell -> cell == Ship NotChecked) b

{- checkWin boardUser boardAI 
    checks if user or AI won
    RETURNS: the player who won first
-}
checkWin :: Board -> Board -> Maybe Player
checkWin boardUser boardAI = case (allShipsChecked boardUser, allShipsChecked boardAI) of
                             (True, True)  -> Just User
                             (True, False) -> Just AI
                             (False, True) -> Just User
                             _             -> Nothing
{- updateStats (statsUser, statsAI) winner
    changes stats according to last winner
    RETURNS: (statsUser, statsAI) updated according to winner

-}
updateStats :: Stats -> Maybe Player -> Stats
updateStats s@((user, n1), (ai, n2)) player = case player of 
                                         Just User -> ((user, n1 + 1), (ai, n2))
                                         Just AI   -> ((user, n1), (ai, n2 + 1))
                                         _         -> s

{- playerShoot game coord 
    shoots the coord for user. Calls AI to shoot. Updates game accordingly
    REUTRNS: game after user and AI have shot. 
-}

playerShoot :: Game -> CellCoord -> Game
playerShoot game coord | validCoordinates coord && not (isChecked (gameBoardAI game) coord)
                        = game {gameBoardAI = shotAIboard, 
                                gameBoardUser = if checkWinner == Just User then gameBoardUser game else shotUserBoard,
                                stackAI = updatedAIstack,
                                winner = checkWinner,
                                gen = newGen,
                                stats = newStats
                                } 
                       | otherwise = game
                       where shotAIboard = checkCell (gameBoardAI game) coord
                             newStats = updateStats (stats game) checkWinner
                             checkWinner = checkWin shotUserBoard shotAIboard
                             ((shotUserBoard, updatedAIstack, updatedHits), newGen) = aiShoot (gameBoardUser game, stackAI game, hitsAI game) (gen game)

---------------------------- Placing AI ------------------------


{- allCoords
       RETURNS: all possible cellcoords on board regarding global n

-}
allCoords :: [CellCoord]
allCoords = [(c, r) | c <- [0..n-1], r <- [0..n-1]]


{- findValidDirectionalPlacements board coords ship direction
       RETURNS: possible coords of ship with direction

-}
findValidDirectionalPlacements :: Board -> [CellCoord] -> ShipSize ->  Direction -> [(CellCoord, Direction)]
findValidDirectionalPlacements b coords s d = map (\coord -> (coord, d)) $ filter (\coord -> validShipPlacement b coord s d) coords
                 

{- findAllValidPlacements board ship 
       RETURNS: all possible placements of ship on board
-}
findAllValidPlacements :: Board -> ShipSize -> [(CellCoord, Direction)]
findAllValidPlacements b s = findValidDirectionalPlacements b allCoords s Horizontal ++ findValidDirectionalPlacements b allCoords s Vertical 


{- randomElement list gen
    RETURNS: (randomly generated element of list, finalGen)
-}
randomElement :: [a] -> StdGen -> (a, StdGen)
randomElement list gen = (list !! randomInt, newGen)
                     where range = (0, length list - 1)
                           (randomInt, newGen) = randomR range gen

{- placeShipAI gen board ship placements
       updates board with a random placement of ship

-}
placeShipAI :: StdGen -> Board -> ShipSize -> [(CellCoord, Direction)] -> (Board, StdGen)
placeShipAI gen b s placements = (placeShipAux b coord s d, newGen)
                             where ((coord , d), newGen) = randomElement placements gen


{- placeMultipleShipsAI gen board ships
       places the ships on random places on the board
       RETURNS: (generated board, newGen)
-}

placeMultipleShipsAI :: StdGen -> Board -> Ships -> (Board, StdGen)
-- VARIANT: length ships
placeMultipleShipsAI gen b [] = (b, gen)
placeMultipleShipsAI gen b ((_, _, s):ships) = placeMultipleShipsAI newGen newBoard ships
                                      where (newBoard, newGen) = placeShipAI gen b s (findAllValidPlacements b s)

listOfBoards :: Int -> StdGen -> Board -> Ships -> [Board]
listOfBoards 0 gen b ships = []
listOfBoards n gen b ships = newBoard  : listOfBoards (n-1) newGen b ships
                           where (newBoard, newGen) = placeMultipleShipsAI gen b ships 


--------------------- Shoot AI --------------------------------

-- Returns column of element in a ShootList
getCol :: (CellCoord,Cell) -> Col
getCol ((a,_),_) = a

-- Returns row of element in a ShootList
getRow :: (CellCoord,Cell) -> Row
getRow ((_,b),_) = b

aiShootList :: Board -> ShootList
aiShootList b = [((c,r), b ! (c,r)) | c <- [0..n-1], r <- [0..n-1]]

-- Creates a ShootList of all cells with state NotChecked
filterShootList :: Board -> StdGen -> (ShootList, StdGen)
filterShootList b gen = (removeChecked (aiPrio shuffledList []), newGen)
    where (shuffledList, newGen) = shuffle (aiShootList b) gen 

-- Sorts a shootlist so AI prioritises cells that are not next to each other
aiPrio :: ShootList -> ShootList -> ShootList
aiPrio [] acc = acc
aiPrio (x:xs) acc 
  | even $ getCol x = if  odd $ getRow x then aiPrio xs (x : acc) else aiPrio xs (acc ++ [x])
  | otherwise       = if even $ getRow x then aiPrio xs (x : acc) else aiPrio xs (acc ++ [x])

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
isShip :: (CellCoord,Cell) -> Bool
isShip (_, Ship NotChecked) = True
isShip _ = False

-- Checks first cell in stack
--PRE: ShootList is not empty
aiShootAux :: (Board,Stack, AIHits) -> ShootList -> (Board,Stack, AIHits)
aiShootAux (b, s@(coord,cell):st, hits)  l | isShip s = (checkCell b coord, removeChecked $ nub (cohesiveCells b s ++ st), checkCell hits coord)
                                           | otherwise = (checkCell b coord, st, hits)

-- Checks first cell in stack
aiShoot :: (Board,Stack, AIHits) -> StdGen -> ((Board, Stack, AIHits), StdGen)
aiShoot (b,s, hits) gen = (aiShootAux (b,removeChecked $ updateStack s newList, hits) newList,newGen)
                     where (newList, newGen) = filterShootList b gen




--------------------- EventHandler --------------------------------

eventHandler :: Event -> Game -> Game
eventHandler (EventKey (SpecialKey KeyEnter) Down _ _) game  = 
     case gameStage game of 
         Placing User -> confirmShip game
         _            -> game

eventHandler (EventKey (SpecialKey key) Down _ _) game       = 
    case gameStage game of 
        Placing User -> moveShip game key
        _            -> game

eventHandler (EventKey (Char 'r') Down _ _) game             = 
    case gameStage game of 
         Placing User -> rotateShip game
         _            -> game

eventHandler (EventKey (MouseButton LeftButton) Up _ mousePos) game =

    case (winner game, gameStage game) of
         (Nothing, Shooting User) -> playerShoot game {shootAnimation = (hitShip (gameBoardAI game) coord,startRadius, mousePos, end, startDerivative, performAnimation)} coord -- should change gamestage to shooting AI
                                                where (_,_,_, end, _, _) = shootAnimation game
                                                      coord = mouseToCell mousePos boardAIPos
                                                      performAnimation = isWithinBoard boardAIPos mousePos && getState (gameBoardAI game) coord == NotChecked
         (_, Shooting User) -> initGame {gameBoardAI = newBoard
                                        , gen = newGen
                                        , currentRound = currentRound game + 1
                                        , stats = stats game
                                        }
                                        where (newBoard, newGen) = placeMultipleShipsAI (gen game) initBoard initShips
         _ -> game
eventHandler _ game = game 
