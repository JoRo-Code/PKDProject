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
--import Test.HUnit




{- validCoordinates coord
    checks if coord is within the range of a board
    PRE: n is Int
    RETURNS: True if coord is in board of size n, else false.
    EXAMPLES:       validCoordinates (0,0)   == True
                    validCoordinates (-1,-1) == False

-}
validCoordinates :: CellCoord -> Bool
validCoordinates  = inRange boardIndex
                    where boardIndex = ((0, 0), (n - 1, n - 1)) 

{- endCoordinates startposition shipsize direction
    calculates the end position of a ship
    PRE: shipsize >=1
    RETURNS: end coordinates of a ship with startposition, shipsize and direction.
    EXAMPLES: 
                endCoordinates (0,0) 1 Horizontal == (0,0)
                endCoordinates (0,0) 5 Horizontal == (4,0)
                endCoordinates (0,0) 5 Vertical   == (0,-4)

-}
endCoordinates :: CellCoord -> ShipSize -> Direction -> CellCoord
endCoordinates (c, r) s Horizontal = (c + s - 1, r)
endCoordinates (c, r) s Vertical = (c, r - s + 1)


---------------------------- Placing ship ----------------------------

surroundingCells :: Board -> CellCoord -> ShipSize -> Direction -> [CellCoord]
surroundingCells b (c,r) s Horizontal =  [(c, r) | r <- [r-1..r+1], c <- [c..c+s-1]] ++ [(c-1, r)] ++ [(c+s,r)]
surroundingCells b (c,r) s Vertical   =  [(c, r) | c <- [c-1..c+1], r <- [r-s+1..r]] ++ [(c, r + 1)] ++ [(c, r -s)]                  

-- Make sure that no ships can be placed within the surrounding cells of another ship
followPlacementRules ::  Board -> CellCoord -> ShipSize -> Direction -> Bool
followPlacementRules b coord s d = all (\coord -> not (validCoordinates coord) || b ! coord /= Ship NotChecked) (surroundingCells b coord s d)


validShipPlacement :: Board ->  CellCoord -> ShipSize -> Direction -> Bool
validShipPlacement b (c, r) s d = validCoordinates (endCoordinates (c, r) s d) -- (-1) because the ship part on (r, c) is included
                                  && validCoordinates (c, r)  
                                  && followPlacementRules b (c,r) s d

placeShipAux :: Board -> CellCoord -> ShipSize -> Direction -> Board
placeShipAux b _ 0 _= b
placeShipAux b (c, r) s Vertical = placeShipAux (b // [((c, r), Ship NotChecked)]) (c, r - 1) (s - 1) Vertical
placeShipAux b (c, r) s Horizontal = placeShipAux (b // [((c, r), Ship NotChecked)]) (c + 1, r) (s - 1) Horizontal

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
    puts current placing ship on board and changes gameStage if all ships have been placed
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


{- hitShip board coord
    checks if coord is a hit on board
    RETURNS: True if coord == Ship NotChecked, and coord is valid. Else False

-}
hitShip :: Board -> CellCoord -> Bool 
hitShip b coord = validCoordinates coord && b ! coord == Ship NotChecked


{- isWithinBoard boardposition screencoord
    checks if a screencoord is within a board with width and height of boardposition
    RETURNS: True if screencoord is within boardposition, else False.

-}
isWithinBoard :: BoardPos -> ScreenCoord -> Bool
isWithinBoard ((x1,y1),(x2,y2)) (x,y) = xNew >= x1 && xNew <= x2 && yNew >= y1 && yNew <= y2
                                      where (xNew, yNew) = (x + 0.5 * screenWidth, y + 0.5*screenHeight)


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
    shows all cellcoords within a global board
    RETURNS: all possible cellcoords on board regarding board size n
    EXAMPLES: allCoords == [(0,0),(0,1),(1,0),(1,1)]
                where n = 2

-}
allCoords :: [CellCoord]
allCoords = [(c, r) | c <- [0..n-1], r <- [0..n-1]]


{- findValidDirectionalPlacements board coords ship direction
       finds possible positions of a ship in a certain direction
       RETURNS: all possible valid coords of ship with direction on board

-}
findValidDirectionalPlacements :: Board -> [CellCoord] -> ShipSize ->  Direction -> [(CellCoord, Direction)]
findValidDirectionalPlacements b coords s d = map (\coord -> (coord, d)) $ filter (\coord -> validShipPlacement b coord s d) coords
                 

{- findAllValidPlacements board ship 
    finds all valid placments of ship on board
    RETURNS: all valid placements of ship on board
-}
findAllValidPlacements :: Board -> ShipSize -> [(CellCoord, Direction)]
findAllValidPlacements b s = findValidDirectionalPlacements b allCoords s Horizontal ++ findValidDirectionalPlacements b allCoords s Vertical 


{- randomElement list gen
    randomly picks an element of list
    RETURNS: (randomly generated element of list with gen, new seed generated with gen)
-}
randomElement :: [a] -> StdGen -> (a, StdGen)
randomElement list gen = (list !! randomInt, newGen)
                     where range = (0, length list - 1)
                           (randomInt, newGen) = randomR range gen

{- placeShipAI gen board ship placements
       updates board with a random placement of ship
       RETURNS: (board with ship integrated randomly with placements and gen, new seed generated with gen)

-}
placeShipAI :: StdGen -> Board -> ShipSize -> [(CellCoord, Direction)] -> (Board, StdGen)
placeShipAI gen b s placements = (placeShipAux b coord s d, newGen)
                             where ((coord , d), newGen) = randomElement placements gen


{- placeMultipleShipsAI gen board ships
       places the ships on random places on the board
       RETURNS: (ships integrated randomly on board with gen, new seed generated with gen)

-}
placeMultipleShipsAI :: StdGen -> Board -> Ships -> (Board, StdGen)
-- VARIANT: length ships
placeMultipleShipsAI gen b [] = (b, gen)
placeMultipleShipsAI gen b ((_, _, s):ships) = placeMultipleShipsAI newGen newBoard ships
                                      where (newBoard, newGen) = placeShipAI gen b s (findAllValidPlacements b s)


--------------------- Shoot AI --------------------------------

{- getCol ((col,row),cell)
   gives the column number of an element in a ShootList or Stack
   RETURNS: col from ((col,row),cell)
   EXAMPLES: getCol ((2,3),Empty NotChecked) == 2
             getCol ((9,0),Ship Checked) == 9
             getCol ((1,1),Ship NotChecked) == 1
-}
getCol :: (CellCoord,Cell) -> Col
getCol ((a,_),_) = a

{- getRow ((col,row),cell)
   gives the row number of an element in a ShootList or Stack
   RETURNS: row from ((col,row),cell)
   EXAMPLES: getCol ((2,3),Empty NotChecked) == 3
             getCol ((9,0),Ship Checked) == 0
             getCol ((1,1),Ship NotChecked) == 1
-}
getRow :: (CellCoord,Cell) -> Row
getRow ((_,b),_) = b

{- aiShootList board
   creates a ShootList from a Board
   RETURNS: a ShootList from board
   EXAMPLES: aiShootList (array ((0,0),(2,2)) [((0,0),Empty NotChecked),
                         ((0,1),Empty Checked),((0,2),Empty NotChecked),
                         ((1,0),Ship Checked),((1,1),Ship Checked),
                         ((1,2),Ship NotChecked),((2,0),Empty NotChecked),
                         ((2,1),Empty Checked),((2,2),Empty NotChecked)]) 
                         == [((0,0),Empty NotChecked),((0,1),Empty Checked),
                            ((0,2),Empty NotChecked),((1,0),Ship Checked),
                            ((1,1),Ship Checked),((1,2),Ship NotChecked),
                            ((2,0),Empty NotChecked),((2,1),Empty Checked),((2,2),Empty NotChecked)]
-}
aiShootList :: Board -> ShootList
aiShootList b = [((c,r), b ! (c,r)) | c <- [0..n-1], r <- [0..n-1]]

{- aiPrio sl acc
   sorts a ShootList so AI prioritises cells that are not next to each other
   RETURNS: a ShootList consisting of sl in prioritised order
   EXAMPLES: filterShootList [((0,0),Empty NotChecked),
                             ((0,1),Empty Checked),((0,2),Empty NotChecked),
                             ((1,0),Ship Checked),((1,1),Ship Checked),
                             ((1,2),Ship NotChecked),((2,0),Empty NotChecked),
                             ((2,1),Empty Checked),((2,2),Empty NotChecked)] []
                             == ([((2,1),Empty Checked),((1,2),Ship NotChecked),
                                ((1,0),Ship Checked),((0,1),Empty Checked),
                                ((0,0),Empty NotChecked),((0,2),Empty NotChecked),
                                ((1,1),Ship Checked),((2,0),Empty NotChecked),((2,2),Empty NotChecked)]
-}
aiPrio :: ShootList -> ShootList -> ShootList
-- VARIANT: length sl
aiPrio [] acc = acc
aiPrio (x:xs) acc 
  | even $ getCol x = if  odd $ getRow x then aiPrio xs (x : acc) else aiPrio xs (acc ++ [x])
  | otherwise       = if even $ getRow x then aiPrio xs (x : acc) else aiPrio xs (acc ++ [x])

{- filterShootList board gen
   creates a ShootList with random order of all cells with state NotChecked from board with gen and sorts by priority
   RETURNS: a tuple consisting of a ShootList of all cells with state NotChecked and StdGen
   EXAMPLES: filterShootList array ((0,0),(2,2)) [((0,0),Empty NotChecked),
                             ((0,1),Empty Checked),((0,2),Empty NotChecked),
                             ((1,0),Ship Checked),((1,1),Ship Checked),
                             ((1,2),Ship NotChecked),((2,0),Empty NotChecked),
                             ((2,1),Empty Checked),((2,2),Empty NotChecked)]) (mkStdGen 10)
                             == ([((1,2),Ship NotChecked),((0,2),Empty NotChecked),
                                ((0,0),Empty NotChecked),((2,2),Empty NotChecked),
                                ((2,0),Empty NotChecked)],
                                StdGen {unStdGen = SMGen 15835914885811367975 614480483733483467})
-}
filterShootList :: Board -> StdGen -> (ShootList, StdGen)
filterShootList b gen = (removeChecked (aiPrio shuffledList []), newGen)
    where (shuffledList, newGen) = shuffle (aiShootList b) gen 

{- removeChecked s
   removes already checked cells from Stack or ShootList
   RETURNS: s without cells that are checked
   EXAMPLES: removeChecked [((0,0),Empty NotChecked),
                           ((0,1),Empty Checked),((0,2),Empty NotChecked),
                           ((1,0),Ship Checked),((1,1),Ship Checked),
                           ((1,2),Ship NotChecked),((2,0),Empty NotChecked),
                           ((2,1),Empty Checked),((2,2),Empty NotChecked)]
                           == [((0,0),Empty NotChecked),((0,2),Empty NotChecked),
                              ((1,2),Ship NotChecked),((2,0),Empty NotChecked),((2,2),Empty NotChecked)]
-}
removeChecked :: [(CellCoord, Cell)] -> [(CellCoord, Cell)]
removeChecked s = filter (\(coord, cell) -> cell == Empty NotChecked || cell == Ship NotChecked) s

{- updateStack stack sl
   updates Stack with the head from ShootList if Stack is empty
   RETURNS: stack with the head from sl if stack is empty, otherwise returns stack
   EXAMPLES: updateStack [] [((0,0),Empty NotChecked),((1,2),Ship NotChecked)] == [((0,0),Empty NotChecked)]
             updateStack [((0,0),Empty NotChecked)] [((1,2),Ship NotChecked)] == [((0,0),Empty NotChecked)]
             updateStack [] [] == []
-}
updateStack :: Stack -> ShootList -> Stack
updateStack [] (x:xs) = [x]
updateStack s _ = s

{- cohesiveCells board ((col,row),cell)
   creates a Stack of the cohesive cells to a cell
   PRE: ((col,row),cell) is in board
   RETURNS: a Stack of the cohesive cells to ((col,row),cell) in board
   EXAMPLES: cohesiveCells (array ((0,0),(2,2)) [((0,0),Empty NotChecked),
                           ((0,1),Empty Checked),((0,2),Empty NotChecked),
                           ((1,0),Ship Checked),((1,1),Ship Checked),
                           ((1,2),Ship NotChecked),((2,0),Empty NotChecked),
                           ((2,1),Empty Checked),((2,2),Empty NotChecked)]) ((1,0),Ship Checked)
                           == [((1,1),Ship Checked),((0,0),Empty NotChecked),((2,0),Empty NotChecked)]
-}
cohesiveCells :: Board -> (CellCoord,Cell) -> Stack
cohesiveCells b ((c,r),x) = map (\coord -> (coord, b ! coord)) (filter validCoordinates [(c,r+1), (c,r-1), (c-1,r), (c+1,r)])

{- isShip ((col,row),cell)
   checks if an element in a ShootList or Stack is Ship NotChecked
   RETURNS: True if cell is Ship NotChecked, otherwise False
   EXAMPLES: isShip ((2,3),Empty NotChecked) == False
             isShip ((9,0),Ship Checked)     == False
             isShip ((1,1),Ship NotChecked)  == True
             isShip ((2,2),Empty Checked)    == False
-}
isShip :: (CellCoord,Cell) -> Bool
isShip (_, Ship NotChecked) = True
isShip _ = False

{- aiShootAux (board,stack,hits) sl
   checks first cell in stack
   PRE: ShootList is not empty
   RETURNS: (board,stack,hits) where first cell in stack is checked in board
   EXAMPLES:
-}
aiShootAux :: (Board,Stack, AIHits) -> ShootList -> (Board,Stack, AIHits)
aiShootAux (b, s@(coord,cell):st, hits)  l | isShip s = (checkCell b coord, removeChecked $ nub (cohesiveCells b s ++ st), checkCell hits coord)
                                           | otherwise = (checkCell b coord, st, hits)

{- aiShootAux (board,stack,hits) gen
   checks first cell in stack
   PRE: ShootList is not empty
   RETURNS: ((board,stack,hits),gen) where first cell in stack is checked in board
   EXAMPLES: 
-}
aiShoot :: (Board,Stack, AIHits) -> StdGen -> ((Board, Stack, AIHits), StdGen)
aiShoot (b,s, hits) gen = (aiShootAux (b,removeChecked $ updateStack s newList, hits) newList,newGen)
                     where (newList, newGen) = filterShootList b gen


[((0,0),Empty NotChecked),((0,1),Empty NotChecked),((1,0),Ship NotChecked),((1,1),Ship Checked)]

--------------------- EventHandler --------------------------------


{- eventHandler event game
    handles all input from user
    RETURNS: game with possible modifications decided by event
-}
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


{- shipsCount board
    counts number of cells with ships NotChecked on a board
    RETURNS: number of cells in board with Ship NotChecked
    EXAMPLES: 
                shipsCount initBoard == 0
-}
shipsCount :: Board -> Int
shipsCount board = length $ filter (==Ship NotChecked) (elems board)


{- cellCount cellContent board
    counts number of cells with cellContent on a board
    RETURNS: number of cells in board with cellContent
    EXAMPLES: 
                cellCount (Ship Checked) initBoard == 0
-}
cellCount ::  Cell -> Board -> Int
cellCount cell board = length $ filter (==cell) (elems board)
----------------------------- TESTCASES --------------------------------
 

-- placing --
--test1 = TestCase $ assertEqual "placeShip: increasing correct ammount of ship-cells on board" (shipsCount initBoard + 5) (shipsCount b)
--        where b = (gameBoardUser (placeShip initGame (0,0) 5 Horizontal))
--test2 = TestCase $ assertEqual "placeShip edgeCase: shipsize 0" initGame (placeShip initGame (0,0) 0 Vertical)
--test3 = TestCase $ assertEqual "placeShip edgeCase: start position outside of board" initGame (placeShip initGame (-1,-1) 0 Vertical)
--test4 = TestCase $ assertEqual "placeShip edgeCase: end position outside of board" initGame (placeShip initGame (9,9) 5 Horizontal)

-- placingAI --
--test5 = TestCase $ assertEqual "placeMultipleShipsAI: correct total ship-cells" 17 (shipsCount b)
--        where b = fst $ placeMultipleShipsAI (mkStdGen 10) initBoard initShips

-- shooting --
--test6 = TestCase $ assertEqual "playerShoot: shooting empty cell " (Empty Checked) (getCell (gameBoardAI (playerShoot initGame (0,0))) (0,0))

--test7 = TestCase $ assertEqual "aiShoot: increasing shot cells" 1  (cellCount (Empty Checked) b)
--        where ((b, _, _),_)= aiShoot (initBoard, [], initBoard) (mkStdGen 10)

--test8 = TestCase $ assertEqual "aiShoot: shooting ship" 1 (cellCount (Ship Checked) b)
--        where ((b, _, _),_)= aiShoot (shipBoard, [], initBoard) (mkStdGen 10)
--              shipBoard = array ((0, 0), (9,9)) $ zip (range ((0, 0), (9,9))) (repeat $ Ship NotChecked)


-- run --
--tests = TestList    [ test1
--                    , test2
--                    , test3
--                    , test4
--                    , test5
--                    , test6
--                    , test7
--                    , test8
--                    ]

--runtests = runTestTT $ tests