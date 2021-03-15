module Logic where
import Shuffle
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Data.Array
import Game
import Data.List
import Test.HUnit

{- cellCount cell board
    counts number of cells with cell on a board
    RETURNS: number of cells in board with cell
    EXAMPLES: 
                cellCount (Ship Checked) initBoard == 0
                cellCount (Empty NotChecked) initBoard == 100
-}
cellCount :: Cell -> Board -> Int
cellCount cell board = length $ filter (==cell) (elems board)

{- shipsCount board
    counts number of cells with Ship NotChecked on a board
    RETURNS: number of cells in board with Ship NotChecked
    EXAMPLES: 
              shipsCount (array ((0,0),(2,2)) [((0,0),Empty NotChecked),
                         ((0,1),Ship NotChecked),((0,2),Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),
                         ((1,2),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Ship NotChecked)]) 
                          == 2
              shipsCount (array ((0,0),(2,2)) [((0,0),Empty NotChecked),
                         ((0,1),Empty NotChecked),((0,2),Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),
                         ((1,2),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked)]) 
                          == 0
-}
shipsCount :: Board -> Int
shipsCount = cellCount (Ship NotChecked)

{- validCoordinates coord
    checks if coord is within the range of a Board
    PRE: global n is BoardSize
    RETURNS: True if coord is in board of size n, else false.
    EXAMPLES:       validCoordinates (0,0)   == True
                    validCoordinates (-1,-1) == False
-}
validCoordinates :: CellCoord -> Bool
validCoordinates  = inRange boardIndex
                    where boardIndex = ((0, 0), (n - 1, n - 1)) 

{- endCoordinates startpos size direction
    calculates the end position of a ship
    PRE: shipsize >=1
    RETURNS: end coordinates of a ship with startpos, size and direction.
    EXAMPLES: 
                endCoordinates (0,0) 1 Horizontal == (0,0)
                endCoordinates (0,0) 5 Horizontal == (4,0)
                endCoordinates (0,0) 5 Vertical   == (0,-4)
-}
endCoordinates :: CellCoord -> ShipSize -> Direction -> CellCoord
endCoordinates (c, r) s Horizontal = (c + s - 1, r)
endCoordinates (c, r) s Vertical = (c, r - s + 1)

---------------------------- Placing ship ----------------------------

{- surroundingCells board coord size direction
   Get all cell coordinates that surrounds a ship starting at coord with size and direction
   RETURNS: A list containing cell coordinates that a ship starting at coord with size and direction
   EXAMPLES: surroundingCells (array ((0,0),(3,3)) ([((0,0),Empty NotChecked),((0,1),Ship NotChecked),((0,2),Ship NotChecked),((0,3)
                                                     ,Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),
                                                     ((1,3),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked),
                                                     ((2,3),Empty NotChecked),((3,0),Empty NotChecked),((3,1),Empty NotChecked),((3,2),Empty NotChecked),
                                                     ((3,3),Empty NotChecked)]))
                                                     (1,3) 3 Vertical == [(0,1),(0,2),(0,3),(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(1,4),(1,0)]
             surroundingCells (array ((0,0),(3,3)) ([((0,0),Empty NotChecked),((0,1),Ship NotChecked),((0,2),Ship NotChecked),((0,3)
                                                     ,Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),
                                                     ((1,3),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked),
                                                     ((2,3),Empty NotChecked),((3,0),Empty NotChecked),((3,1),Empty NotChecked),((3,2),Empty NotChecked),
                                                     ((3,3),Empty NotChecked)]))
                                                     (1,3) 3 Horizontal == [(1,2),(2,2),(3,2),(1,3),(2,3),(3,3),(1,4),(2,4),(3,4),(0,3),(4,3)]
-}
surroundingCells :: Board -> CellCoord -> ShipSize -> Direction -> [CellCoord]
surroundingCells b (c,r) s Horizontal =  [(c, r) | r <- [r-1..r+1], c <- [c..c+s-1]] ++ [(c-1,r), (c+s,r)]
surroundingCells b (c,r) s Vertical   =  [(c, r) | c <- [c-1..c+1], r <- [r-s+1..r]] ++ [(c,r+1), (c,r-s)]                  


{- followPlacementRules board coord size direction
   Check if ship placement follows the games placement rules
   RETURNS: True if placement of a ship with size and direction starting at coord,
            follows the placement rules of the game, else False.
   EXAMPLES: followPlacementRules (array ((0,0),(3,3)) [((0,0),Empty NotChecked),((0,1),Ship NotChecked),((0,2),Ship NotChecked),((0,3)
                                                     ,Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),
                                                     ((1,3),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked),
                                                     ((2,3),Empty NotChecked),((3,0),Empty NotChecked),((3,1),Empty NotChecked),((3,2),Empty NotChecked),
                                                     ((3,3),Empty NotChecked)])
                                                     (1,3) 3 Vertical == False
             followPlacementRules (array ((0,0),(3,3)) [((0,0),Empty NotChecked),((0,1),Ship NotChecked),((0,2),Ship NotChecked),((0,3)
                                                     ,Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),
                                                     ((1,3),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked),
                                                     ((2,3),Empty NotChecked),((3,0),Empty NotChecked),((3,1),Empty NotChecked),((3,2),Empty NotChecked),
                                                     ((3,3),Empty NotChecked)])
                                                     (1,3) 3 Horizontal == True
-}
followPlacementRules ::  Board -> CellCoord -> ShipSize -> Direction -> Bool
followPlacementRules b coord s d = all (\coord -> not (validCoordinates coord) || b ! coord /= Ship NotChecked) (surroundingCells b coord s d)

{- validShipPlacement coord size direction
   Check if ship placement is valid
   RETURNS: True if placement of a ship with size and direction starting at coord, is valid, else False.
   EXAMPLES: validShipPlacement (array ((0,0),(3,3)) [((0,0),Empty NotChecked),((0,1),Ship NotChecked),((0,2),Ship NotChecked),((0,3),
                                Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),
                                ((1,3),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked),
                                ((2,3),Empty NotChecked),((3,0),Empty NotChecked),((3,1),Empty NotChecked),((3,2),Empty NotChecked),
                                ((3,3),Empty NotChecked)])
                                (0,0) 3 Horizontal 
                                == False
             validShipPlacement (array ((0,0),(3,3)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),
                                ((0,3),Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),
                                ((1,3),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked),
                                ((2,3),Empty NotChecked),((3,0),Empty NotChecked),((3,1),Empty NotChecked),((3,2),Empty NotChecked),
                                ((3,3),Empty NotChecked)]) (1,1) 2 Vertical 
                                == True
-}
validShipPlacement :: Board ->  CellCoord -> ShipSize -> Direction -> Bool
validShipPlacement b (c, r) s d = validCoordinates (endCoordinates (c, r) s d)
                                  && validCoordinates (c, r)  
                                  && followPlacementRules b (c,r) s d

{- placeShipAux board coord size direction
   Places a ship with size and direction, on board starting at coord
   PRE: Placement of ship is valid, size is >= 0
   RETURNS: updated board where ship with size and direction, starting at coord, have been placed.
   EXAMPLES: placeShipAux (array ((0,0),(3,3)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),((0,3),
                                                Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),
                                                ((1,3),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked),
                                                ((2,3),Empty NotChecked),((3,0),Empty NotChecked),((3,1),Empty NotChecked),((3,2),Empty NotChecked),
                                                ((3,3),Empty NotChecked)])
                                                (0,0) 4 Horizontal == array ((0,0),(3,3)) [((0,0),Ship NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),
                                                                      ((0,3),Empty NotChecked),((1,0),Ship NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),
                                                                      ((1,3),Empty NotChecked),((2,0),Ship NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked),
                                                                      ((2,3),Empty NotChecked),((3,0),Ship NotChecked),((3,1),Empty NotChecked),((3,2),Empty NotChecked),
                                                                      ((3,3),Empty NotChecked)]
-}
placeShipAux :: Board -> CellCoord -> ShipSize -> Direction -> Board
-- VARIANT: size
placeShipAux b _ 0 _ = b
placeShipAux b (c, r) s Vertical = placeShipAux (b // [((c, r), Ship NotChecked)]) (c, r - 1) (s - 1) Vertical
placeShipAux b (c, r) s Horizontal = placeShipAux (b // [((c, r), Ship NotChecked)]) (c + 1, r) (s - 1) Horizontal

{- placeShip game coord size direction
   Places a ship with size and direction on board starting at coord
   PRE: Placement of ship is valid
   RETURNS: If placement is valid then it returns game where ship with size, direction,
            starting at coord have been placed in gameBoardUser, otherwise game unchanged.
   EXAMPLES: 
                placeShip initGame {gameBoardAI = initBoard} (0,0) 0 Horizontal     == initGame {gameBoardAI = initBoard}
                placeShip initGame {gameBoardAI = initBoard} (-1,0) 5 Horizontal    == initGame {gameBoardAI = initBoard}
                placeShip initGame {gameBoardAI = initBoard} (0,0) 3 Horizontal     == Game { gameBoardUser = array ((0,0),(2,2)) [((0,0),Ship NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),((1,0),Ship NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),((2,0),Ship NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked)]
                                                                                            , gameBoardAI = array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked)]
                                                                                            , gameStage = Placing User, shipsUser = [((0,3),Vertical,4),((0,0),Horizontal,3),((0,2),Vertical,3),((0,0),Horizontal,2)]
                                                                                            , stackAI = []
                                                                                            , winner = Nothing
                                                                                            , gen = StdGen {unStdGen = SMGen 16626775891238333538 2532601429470541125}
                                                                                            , currentRound = 1
                                                                                            , stats = ((User,0),(AI,0))
                                                                                            , shootAnimation = (False,0.0,(720.0,285.0),95.0,720.0,False)
                                                                                            , radarAnimation = ([285.0,228.0,171.0,114.0,57.0],0.0)
                                                                                            }
-}
placeShip :: Game -> CellCoord -> ShipSize -> Direction -> Game
placeShip game _ 0 _= game
placeShip game coord s d | validShipPlacement board coord s d = 
                           game {gameBoardUser = placeShipAux board coord s d,
                                 shipsUser     = tail $ shipsUser game}
                         | otherwise = game
                         where board = gameBoardUser game

---------------------------- Moving Ship Picture ---------------------

{- mouseToCell mousePos boardPos
    calculates current cell from mouseposition
    RETURNS: the corresponding cellCoordinates for mousePos on a board with boardPos
    EXAMPLES: mouseToCell (-200, 280) ((0.0,0.0),(600.0,600.0)) == (9, 9), (when screenWidth == 1440, screenDivider == 300, n == 10)
              mouseToCell (200, -280) ((900.0,0.0),(1500.0,600.0)) == (0, 0), (when screenWidth == 1440, screenDivider == 300, n == 10)
-}
mouseToCell :: ScreenCoord -> BoardPos -> CellCoord
mouseToCell (x, y) boardPos@((x1,y1),(x2,y2)) =  (floor ((x - x1 + boardWidth + screenDivider * 0.5) / cellWidth), floor ((y - y1 + boardHeight  * 0.5) / cellHeight ))


{- moveShip game key
    updates the first placing ship's cellCoordinates according to key input
    RETURNS: If valid position then head of shipsUser game with new coordinates according to key, else game.
    EXAMPLES:
                moveShip initGame {gameBoardAI = initBoard} KeySpace                                        == initGame {gameBoardAI = initBoard} 
                moveShip initGame {gameBoardAI = initBoard, shipsUser = [((1,0), Horizontal,5)]} KeyLeft    == initGame {gameBoardAI = initBoard, shipsUser = [((0,0), Horizontal,5)]}
                moveShip initGame {gameBoardAI = initBoard, shipsUser = [((0,0), Horizontal,5)]} KeyLeft    == initGame {gameBoardAI = initBoard, shipsUser = [((0,0), Horizontal,5)]}
                moveShip initGame {gameBoardAI = initBoard, shipsUser = [((1,0), Horizontal,5)]} KeyRight   == initGame {gameBoardAI = initBoard, shipsUser = [((2,0), Horizontal,5)]}
                moveShip initGame {gameBoardAI = initBoard, shipsUser = [((9,0), Horizontal,5)]} KeyRight   == initGame {gameBoardAI = initBoard, shipsUser = [((9,0), Horizontal,5)]}
                moveShip initGame {gameBoardAI = initBoard, shipsUser = [((1,0), Horizontal,5)]} KeyUp      == initGame {gameBoardAI = initBoard, shipsUser = [((1,1), Horizontal,5)]}
                moveShip initGame {gameBoardAI = initBoard, shipsUser = [((1,9), Horizontal,5)]} KeyUp      == initGame {gameBoardAI = initBoard, shipsUser = [((1,9), Horizontal,5)]}
                moveShip initGame {gameBoardAI = initBoard, shipsUser = [((1,1), Horizontal,5)]} KeyDown    == initGame {gameBoardAI = initBoard, shipsUser = [((1,0), Horizontal,5)]}
                moveShip initGame {gameBoardAI = initBoard, shipsUser = [((1,0), Horizontal,5)]} KeyDown    == initGame {gameBoardAI = initBoard, shipsUser = [((1,0), Horizontal,5)]}
-}
moveShip :: Game -> SpecialKey -> Game
moveShip game keyDir | validCoordinates (endCoordinates newCoord s d)
                       && validCoordinates newCoord = game {shipsUser = (newCoord, d, s) : ships}
                     | otherwise = game
                      where (((c, r), d, s):ships) = shipsUser game
                            newCoord =  case keyDir of
                                        KeyLeft  -> (c - 1, r)
                                        KeyRight -> (c + 1, r)  
                                        KeyUp    -> (c, r + 1)
                                        KeyDown  -> (c, r - 1)
                                        _        -> (c, r)

{- rotateShip game 
    changes the current placing ship's direction
    PRE: shipsUser in game not empty
    RETURNS: If opposite direction is valid then head of shipsUser game with opposite direction, else game.
    EXAMPLES: 
                rotateShip initGame {gameBoardAI = initBoard}                                       == initGame {gameBoardAI = initBoard}
                rotateShip initGame {gameBoardAI = initBoard, shipsUser = [((0,9), Horizontal,5)]}  == initGame {gameBoardAI = initBoard, shipsUser = [((0,9), Vertical,5)]}
                rotateShip initGame {gameBoardAI = initBoard, shipsUser = [((0,9), Vertical,5)]}    == initGame {gameBoardAI = initBoard, shipsUser = [((0,9), Horizontal,5)]}
                rotateShip initGame {gameBoardAI = initBoard, shipsUser = [((9,0), Vertical,5)]}    == initGame {gameBoardAI = initBoard, shipsUser = [((9,0), Vertical,5)]} 
                rotateShip initGame {gameBoardAI = initBoard, shipsUser = [((0,0), Horizontal,5)]}  == initGame {gameBoardAI = initBoard, shipsUser = [((0,0), Horizontal,5)]} 
-}
rotateShip :: Game -> Game
rotateShip game | validCoordinates $ endCoordinates coord s newDirection =
                  game {shipsUser = (coord, newDirection, s) : ships}
                | otherwise = game
                  where ((coord, d, s):ships) = shipsUser game
                        newDirection = case d of
                                      Horizontal -> Vertical
                                      Vertical   -> Horizontal

{- confirmShip game
    puts current placing ship on board and changes gameStage if all ships have been placed
    RETURNS: Game where gameBoardUser have been updated with current placing ship, 
             if placement is valid, else returns game unchanged
    EXAMPLES: 
                confirmShip initGame {gameBoardAI = initBoard, shipsUser = [((-1,0), Horizontal,5)] } == initGame {gameBoardAI = initBoard, shipsUser = [((-1,0), Horizontal,5)]} 
                confirmShip initGame {gameBoardAI = initBoard } == Game { gameBoardUser = array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked)]
                                                                        , gameBoardAI = array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked)]
                                                                        , gameStage = Placing User
                                                                        , shipsUser = [((0,0),Horizontal,5),((0,3),Vertical,4),((0,0),Horizontal,3),((0,2),Vertical,3),((0,0),Horizontal,2)]
                                                                        , stackAI = []
                                                                        , winner = Nothing
                                                                        , gen = StdGen {unStdGen = SMGen 16626775891238333538 2532601429470541125}
                                                                        , currentRound = 1
                                                                        , stats = ((User,0),(AI,0))
                                                                        , shootAnimation = (False,0.0,(720.0,285.0),95.0,720.0,False)
                                                                        , radarAnimation = ([285.0,228.0,171.0,114.0,57.0],0.0)
                                                                        }
-}
confirmShip :: Game -> Game
confirmShip game | validShipPlacement board coord s d = 
                   updatedGame {gameStage = newGameStage}
                 | otherwise = game
                   where ((coord, d, s):ships) = shipsUser game
                         board = gameBoardUser game
                         updatedGame = placeShip game coord s d
                         newGameStage = if null ships then Shooting User else Placing User


---------------------------- Shooting Ship ------------------------

{- getCell board coord
    extracts board contents on a specific column and row
    PRE: coord in board
    RETURNS: content of coord in board
    EXAMPLES: getCell (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),((1,0),Empty NotChecked),((1,1),Empty NotChecked)]) (0,1) == Ship Checked
              getCell (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),((1,0),Empty NotChecked),((1,1),Empty NotChecked)]) (0,0) == Empty NotChecked
-}
getCell :: Board -> CellCoord -> Cell
getCell b c = b ! c

{- checkCell board coord
    Changes the state of a cell to checked
    PRE: coord in board
    RETURNS: if coord == NotChecked then board with checked coord, else board.
    EXAMPLES: checkCell  (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),
                         ((1,0),Empty NotChecked),((1,1),Empty NotChecked)]) (0, 0) 
                         == array ((0,0),(1,1)) [((0,0),Empty Checked),((0,1),Ship Checked),
                                  ((1,0),Empty NotChecked),((1,1),Empty NotChecked)]
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
    EXAMPLES: getState (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),
                       ((1,0),Empty NotChecked),((1,1),Empty NotChecked)]) (0, 0) == NotChecked
              getState (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),
                       ((1,0),Empty NotChecked),((1,1),Empty NotChecked)]) (0, 1) == Checked
-}
getState :: Board -> CellCoord -> SquareState
getState b c =  case getCell b c of
                     Empty s -> s
                     Ship s -> s

{- hitShip board coord
    checks if coord is a hit on board
    RETURNS: True if coord == Ship NotChecked, and coord is valid. Else False
    EXAMPLES: hitShip (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),
                      ((1,0),Empty NotChecked),((1,1),Empty NotChecked)]) (0, 0) == False
              hitShip (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship NotChecked),
                      ((1,0),Empty NotChecked),((1,1),Empty NotChecked)]) (0, 1) == True
-}
hitShip :: Board -> CellCoord -> Bool 
hitShip b coord = validCoordinates coord && b ! coord == Ship NotChecked

{- isWithinBoard boardposition screencoord
    checks if a screencoord is within a board with width and height of boardposition
    RETURNS: True if screencoord is within boardposition, else False.
    EXAMPLES: isWithinBoard ((900.0,0.0),(1500.0,600.0)) (200, 100) == True (when screenWidth == 1440, screenDivider == 300)
              isWithinBoard ((900.0,0.0),(1500.0,600.0)) (0, 0)     == False (when screenWidth == 1440, screenDivider == 300)
-}
isWithinBoard :: BoardPos -> ScreenCoord -> Bool
isWithinBoard ((x1,y1),(x2,y2)) (x,y) = xNew >= x1 && xNew <= x2 && yNew >= y1 && yNew <= y2
                                      where (xNew, yNew) = (x + 0.5 * screenWidth, y + 0.5*screenHeight)

{- isChecked board coord
    evaluates if coord is checked
    PRE: coord in board
    RETURNS: True if coord == Checked, else False.
    EXAMPLES: isChecked (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),
                        ((1,0),Empty NotChecked),((1,1),Empty NotChecked)]) (0,0) == False
              isChecked (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),
                        ((1,0),Empty NotChecked),((1,1),Empty NotChecked)]) (0,1) == True
-}
isChecked :: Board -> CellCoord -> Bool
isChecked b coord = s == Checked
        where s = getState b coord

{- allShipsChecked board 
    checks if all ships on board are Checked
    RETURNS: True if all ships are checked on board, else false
    EXAMPLES: allShipsChecked (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),
                              ((1,0),Empty NotChecked),((1,1), Ship NotChecked)]) == False
              allShipsChecked (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),
                              ((1,0),Empty NotChecked),((1,1), Ship Checked)]) == True
-}
allShipsChecked :: Board -> Bool
allShipsChecked b = not $ any (\cell -> cell == Ship NotChecked) b

{- checkWin boardUser boardAI 
    checks if user or AI has won
    RETURNS: the player who won first, if no winner it returns Nothing
    EXAMPLES: checkWin (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),
                       ((1,0), Ship Checked),((1,1), Ship Checked)])
                       (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),
                       ((1,0), Ship Checked),((1,1), Ship Checked)])
                       == Just User
              checkWin (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),
                       ((1,0), Ship Checked),((1,1), Ship Checked)])
                       (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),
                       ((1,0), Ship NotChecked),((1,1), Ship Checked)])
                       == Just AI
              checkWin (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship NotChecked),
                       ((1,0), Ship Checked),((1,1), Ship Checked)])
                       (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),
                       ((1,0), Ship NotChecked),((1,1), Ship NotChecked)])
                       == Nothing
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
    EXAMPLES: updateStats ((User, 1), (AI, 1)) (Just User) == ((User, 2), (AI, 1))
              updateStats ((User, 1), (AI, 1)) (Just AI)   == ((User, 1), (AI, 2))
              updateStats ((User, 1), (AI, 1)) Nothing     == ((User, 1), (AI, 1))
-}
updateStats :: Stats -> Maybe Player -> Stats
updateStats s@((user, n1), (ai, n2)) player = case player of 
                                         Just User -> ((user, n1 + 1), (ai, n2))
                                         Just AI   -> ((user, n1), (ai, n2 + 1))
                                         _         -> s

{- playerShoot game coord 
    shoots the coord for user. Calls AI to shoot. Updates game accordingly
    RETURNS: game after AIboard have been shot at coord, and AI have shot on the user's board
    EXAMPLES:   playerShoot initGame {gameBoardAI = initBoard } (-1,0)   == initGame {gameBoardAI = initBoard } 
                playerShoot initGame {gameBoardAI = initBoard } (0,0)    == game =  { gameBoardUser = array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked)
                                                                                                                            , ((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked)
                                                                                                                            , ((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked)]
                                                                                    , gameBoardAI = array ((0,0),(2,2)) [((0,0),Empty Checked),((0,1),Empty NotChecked),((0,2),Empty NotChecked)
                                                                                                                            , ((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked)
                                                                                                                            , ((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked)] 
                                                                                    , gameStage = Placing User
                                                                                    , shipsUser = [((0,0),Horizontal,5),((0,3),Vertical,4),((0,0),Horizontal,3),((0,2),Vertical,3),((0,0),Horizontal,2)]
                                                                                    , stackAI = []
                                                                                    , winner = Just User
                                                                                    , gen = StdGen {unStdGen = SMGen 2526700609054100431 2532601429470541125}
                                                                                    , currentRound = 1
                                                                                    , stats = ((User,1),(AI,0))
                                                                                    , shootAnimation = (False,0.0,(720.0,285.0),95.0,720.0,False)
                                                                                    , radarAnimation = ([285.0,228.0,171.0,114.0,57.0],0.0)
                                                                                    }
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
                             ((shotUserBoard, updatedAIstack), newGen) = aiShoot (gameBoardUser game, stackAI game) (gen game)

---------------------------- Placing AI ------------------------

{- allCoords
    shows all cellcoords within a global board
    RETURNS: all possible cellcoords on board regarding board size n
    EXAMPLES: allCoords == [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(0,8),(0,9),(1,0),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),
                           (1,7),(1,8),(1,9),(2,0),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(2,9),(3,0),(3,1),(3,2),(3,3),
                           (3,4),(3,5),(3,6),(3,7),(3,8),(3,9),(4,0),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(4,7),(4,8),(4,9),(5,0),
                           (5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(5,7),(5,8),(5,9),(6,0),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6),(6,7),
                           (6,8),(6,9),(7,0),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,7),(7,8),(7,9),(8,0),(8,1),(8,2),(8,3),(8,4),
                           (8,5),(8,6),(8,7),(8,8),(8,9),(9,0),(9,1),(9,2),(9,3),(9,4),(9,5),(9,6),(9,7),(9,8),(9,9)]
-}
allCoords :: [CellCoord]
allCoords = [(c, r) | c <- [0..n-1], r <- [0..n-1]]

{- findValidDirectionalPlacements board coords size direction
   finds possible positions of a ship in a certain direction
   RETURNS: all possible valid coords of ship, with size and direction, on board
   EXAMPLES: findValidDirectionalPlacements (array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),
                                            ((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),
                                            ((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked)]) 
                                            [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)] 3 Horizontal 
                                            == [((0,0),Horizontal),((0,1),Horizontal),((0,2),Horizontal)]
             findValidDirectionalPlacements (array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),
                                            ((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),
                                            ((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked)])
                                            [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)] 2 Vertical
                                            == [((0,1),Vertical),((0,2),Vertical),((1,1),Vertical),((1,2),Vertical),((2,1),Vertical),((2,2),Vertical)]
-}
findValidDirectionalPlacements :: Board -> [CellCoord] -> ShipSize ->  Direction -> [(CellCoord, Direction)]
findValidDirectionalPlacements b coords s d = map (\coord -> (coord, d)) $ filter (\coord -> validShipPlacement b coord s d) coords
                 
{- findAllValidPlacements board ship 
    finds all valid placements of ship on board
    RETURNS: all valid placements of ship on board
    EXAMPLES: findAllValidPlacements (array ((0,0),(2,2)) [((0,0),Empty NotChecked),
              ((0,1),Empty NotChecked),((0,2),Empty NotChecked),((1,0),Empty NotChecked),
              ((1,1),Empty NotChecked),((1,2),Empty NotChecked),((2,0),Empty NotChecked),
              ((2,1),Empty NotChecked),((2,2),Empty NotChecked)]) 3
              == [((0,0),Horizontal),((0,1),Horizontal),((0,2),Horizontal),
                 ((0,2),Vertical),((1,2),Vertical),((2,2),Vertical)]
              findAllValidPlacements (array ((0,0),(2,2)) [((0,0),Ship NotChecked),
              ((0,1),Ship NotChecked),((0,2),Empty NotChecked),((1,0),Empty NotChecked),
              ((1,1),Empty NotChecked),((1,2),Empty NotChecked),((2,0),Empty NotChecked),
              ((2,1),Empty NotChecked),((2,2),Empty NotChecked)]) 3
              == [((2,2),Vertical)]
-}
findAllValidPlacements :: Board -> ShipSize -> [(CellCoord, Direction)]
findAllValidPlacements b s = findValidDirectionalPlacements b allCoords s Horizontal ++ findValidDirectionalPlacements b allCoords s Vertical 

{- randomElement list gen
    randomly picks an element of list
    RETURNS: (randomly generated element of list with gen, new seed generated with gen)
    EXAMPLES: fst (randomElement [1,2,3,4,5,6,7,8] (mkStdGen 10)) == 7
              fst (randomElement [1,2,3,4,5,6,7,8] (mkStdGen 11)) == 4
              fst (randomElement ['a','b','c','d','e'] (mkStdGen 10)) == 'd'
-}
randomElement :: [a] -> StdGen -> (a, StdGen)
randomElement list gen = (list !! randomInt, newGen)
                     where range = (0, length list - 1)
                           (randomInt, newGen) = randomR range gen

{- placeShipAI gen board ship placements
       updates board with a random placement of ship
       RETURNS: (board with ship integrated randomly with placements and gen, new seed generated with gen)
       EXAMPLES: fst (placeShipAI (mkStdGen 10) b 3 (findAllValidPlacements b 3)
                     where b = (array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),
                     ((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),((2,0),Empty NotChecked),
                     ((2,1),Empty NotChecked),((2,2),Empty NotChecked)])
                     == (array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),
                        ((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),((2,0),Ship NotChecked),
                        ((2,1),Ship NotChecked),((2,2),Ship NotChecked)])
-}
placeShipAI :: StdGen -> Board -> ShipSize -> [(CellCoord, Direction)] -> (Board, StdGen)
placeShipAI gen b s placements = (placeShipAux b coord s d, newGen)
                             where ((coord , d), newGen) = randomElement placements gen

{- placeMultipleShipsAI gen board ships
       places the ships on random places on the board
       RETURNS: (ships integrated randomly on board with gen, new seed generated with gen)
       EXAMPLES: fst (placeMultipleShipsAI (mkStdGen 10) (array ((0,0),(2,2)) [((0,0),Empty NotChecked),
                 ((0,1),Empty NotChecked),((0,2),Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),
                 ((1,2),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked)]) 
                 [((1,2), Horizontal, 2),((0,0), Vertical, 2)])
                 == (array ((0,0),(2,2)) [((0,0),Ship NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),
                    ((1,0),Ship NotChecked),((1,1),Empty NotChecked),((1,2),Ship NotChecked),((2,0),Empty NotChecked),
                    ((2,1),Empty NotChecked),((2,2),Ship NotChecked)])
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
   EXAMPLES: getRow ((2,3),Empty NotChecked) == 3
             getRow ((9,0),Ship Checked) == 0
             getRow ((1,1),Ship NotChecked) == 1
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
aiShootList = assocs

{- aiPrio sl acc
   sorts a ShootList so AI prioritises cells that are not next to each other
   RETURNS: acc; a ShootList consisting of sl in prioritised order
   EXAMPLES: aiPrio [((0,0),Empty NotChecked),
                    ((0,1),Empty Checked),((0,2),Empty NotChecked),
                    ((1,0),Ship Checked),((1,1),Ship Checked),
                    ((1,2),Ship NotChecked),((2,0),Empty NotChecked),
                    ((2,1),Empty Checked),((2,2),Empty NotChecked)] []
                    == ([((2,1),Empty Checked),((1,2),Ship NotChecked),
                       ((1,0),Ship Checked),((0,1),Empty Checked),
                       ((0,0),Empty NotChecked),((0,2),Empty NotChecked),
                       ((1,1),Ship Checked),((2,0),Empty NotChecked),((2,2),Empty NotChecked)])
-}
aiPrio :: ShootList -> ShootList -> ShootList
-- VARIANT: length sl
aiPrio [] acc = acc
aiPrio (x:xs) acc 
  | even $ getCol x = if  odd $ getRow x then aiPrio xs (x : acc) else aiPrio xs (acc ++ [x])
  | otherwise       = if even $ getRow x then aiPrio xs (x : acc) else aiPrio xs (acc ++ [x])

{- filterShootList board gen
   creates a ShootList with random order of all cells with state NotChecked from board with gen and sorts by priority
   RETURNS: a tuple consisting of a ShootList of all cells with state NotChecked on board, and StdGen from gen
   EXAMPLES: fst (filterShootList (array ((0,0),(2,2)) [((0,0),Empty NotChecked),
                                  ((0,1),Empty Checked),((0,2),Empty NotChecked),
                                  ((1,0),Ship Checked),((1,1),Ship Checked),
                                  ((1,2),Ship NotChecked),((2,0),Empty NotChecked),
                                  ((2,1),Empty Checked),((2,2),Empty NotChecked)]) (mkStdGen 10))
                                  == ([((1,2),Ship NotChecked),((0,2),Empty NotChecked),
                                     ((0,0),Empty NotChecked),((2,2),Empty NotChecked),s
                                     ((2,0),Empty NotChecked)])
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
             removeChecked [((0,0),Empty NotChecked),
                           ((0,1),Empty NotChecked),((0,2),Empty NotChecked),
                           ((1,0),Ship NotChecked),((1,1),Ship NotChecked),
                           ((1,2),Ship NotChecked),((2,0),Empty NotChecked),
                           ((2,1),Empty NotChecked),((2,2),Empty NotChecked)]
                           == [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),
                              ((1,0),Ship NotChecked),((1,1),Ship NotChecked),((1,2),Ship NotChecked),
                              ((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked)]
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
             cohesiveCells (array ((0,0),(2,2)) [((0,0), Ship  NotChecked),((0,1),Empty Checked),((0,2),
                            Ship NotChecked),((1,0),Ship Checked),((1,1),Ship Checked),((1,2),Ship NotChecked),
                            ((2,0),Empty NotChecked),((2,1),Empty Checked),((2,2),Empty NotChecked)]) ((1,0),Ship Checked)
                            == [((1,1),Ship Checked),((0,0),Ship NotChecked),((2,0),Empty NotChecked)]
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

{- aiShootAux (board,stack)
   checks first cell in stack
   PRE: Stack is not empty
   RETURNS: (board,stack) where first cell in stack is checked in board
   EXAMPLES: aiShootAux (array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),
                        ((1,0),Empty NotChecked),((1,1),Empty NotChecked)],[((1,0),Empty NotChecked)]) 
                        == (array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),
                           ((1,0),Empty Checked),((1,1),Empty NotChecked)],[])
-}
aiShootAux :: (Board,Stack) -> (Board,Stack)
aiShootAux (b, s@(coord,cell):st) | isShip s = (checkCell b coord, removeChecked $ nub (cohesiveCells b s ++ st))
                                  | otherwise = (checkCell b coord, st)

{- aiShoot (board,stack) gen
   checks first cell in stack
   PRE: Board has NotChecked cells
   RETURNS: ((board,stack),gen) where first cell in stack is checked in board
   EXAMPLES: fst (aiShoot (array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),
                          ((1,0),Empty NotChecked),((1,1),Empty NotChecked)],
                          [((1,0),Empty NotChecked)]) (mkStdGen 10)) 
                          == ((array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),
                             ((1,0),Empty Checked),((1,1),Empty NotChecked)],[]))
             fst (aiShoot (array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),
                          ((1,0),Empty NotChecked),((1,1),Empty NotChecked)],
                          []) (mkStdGen 10)) 
                          == ((array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),
                             ((1,0),Empty Checked),((1,1),Empty NotChecked)],[]))
-}
aiShoot :: (Board,Stack) -> StdGen -> ((Board, Stack), StdGen)
aiShoot (b,s) gen = (aiShootAux (b,removeChecked $ updateStack s newList),newGen)
                     where (newList, newGen) = filterShootList b gen

--------------------- EventHandler --------------------------------

{- eventHandler event game
    handles all input from user
    RETURNS: game with possible modifications decided by event
-}
eventHandler :: Event -> Game -> Game
eventHandler (EventKey (key) Down _ _) game =           -- controlling placingShip with keys
    case gameStage game of 
        Placing User -> case key of
                            SpecialKey KeyEnter -> confirmShip game
                            SpecialKey arrowkey -> moveShip game arrowkey
                            Char 'r'            -> rotateShip game
                            _                   -> game

        _           -> game

eventHandler (EventKey (MouseButton LeftButton) Up _ mousePos) game =   -- shooting with mouseclick
    case (winner game, gameStage game) of
         (Nothing, Shooting User) -> playerShoot game { shootAnimation =   ( hitShip (gameBoardAI game) coord -- determines color of explosion
                                                                           , startRadius
                                                                           , mousePos
                                                                           , end
                                                                           , startDerivative
                                                                           , performAnimation
                                                                           )
                                                      } coord 
                                                where (_,_,_, end, _, _)   = shootAnimation game
                                                      coord                = mouseToCell mousePos boardAIPos
                                                      performAnimation     = isWithinBoard boardAIPos mousePos 
                                                                              && getState (gameBoardAI game) coord == NotChecked
         (_, Shooting User)       -> initGame   { gameBoardAI  = newBoard
                                                , gen          = newGen
                                                , currentRound = currentRound game + 1
                                                , stats        = stats game
                                                }
                                                where (newBoard, newGen) = placeMultipleShipsAI (gen game) initBoard initShips
         _                        -> game

eventHandler _ game = game 
----------------------------- TESTCASES --------------------------------


-- general logic (A) --
test1A = TestCase $ assertEqual "shipsCount: initBoard" 0 (shipsCount initBoard)
test2A = TestCase $ assertEqual "cellCount: initBoard" 0 (cellCount (Ship Checked) initBoard)
test3A = TestCase $ assertEqual "validCoordinates: coordinates out of bounds when n = 10" False (validCoordinates (2,11))
test4A = TestCase $ assertEqual "validCoordinates: negative coordinates" False (validCoordinates (-1,-1))
test5A = TestCase $ assertEqual "validCoordinates: valid coordinates" True (validCoordinates (0,0))
test6A = TestCase $ assertEqual "endCoordinates: vertical placement" (5,4) (endCoordinates (5,6) 3 Vertical)
test7A = TestCase $ assertEqual "endCoordinates: ship out of bounds" (0,-4) (endCoordinates (0,0) 5 Vertical)

-- placing user (B) --
test1B = TestCase $ assertEqual "surroundingCells: horizontal placement" [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(0,0),(1,0),(2,0),(3,0),(4,0),(0,1),(1,1),(2,1),(3,1),(4,1),(-1,0),(5,0)] (surroundingCells initBoard (0,0) 5 Horizontal)
test2B = TestCase $ assertEqual "followPlacementRules: vertical valid placement" True (followPlacementRules initBoard (5,6) 3 Vertical)
test3B = TestCase $ assertEqual "followPlacementRules: invalid placement" False (followPlacementRules (array ((0,0),(3,3)) [((0,0),Empty NotChecked),((0,1),Ship NotChecked),((0,2),Ship NotChecked),((0,3),Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),((1,3),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked),((2,3),Empty NotChecked),((3,0),Empty NotChecked),((3,1),Empty NotChecked),((3,2),Empty NotChecked),((3,3),Empty NotChecked)])(1,3) 3 Vertical)
test4B = TestCase $ assertEqual "validShipPlacement: ship out of bounds" False (validShipPlacement initBoard (8,0) 3 Vertical)
test5B = TestCase $ assertEqual "validShipPlacement: valid placement" True (validShipPlacement (array ((0,0),(3,3)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),((0,3),Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),((1,3),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked),((2,3),Empty NotChecked),((3,0),Empty NotChecked),((3,1),Empty NotChecked),((3,2),Empty NotChecked),((3,3),Empty NotChecked)]) (1,1) 2 Vertical)
test6B = TestCase $ assertEqual "placeShipAux: place a horizontal ship" (array ((0,0),(9,9)) [((0,0),Ship NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),((0,3),Empty NotChecked),((0,4),Empty NotChecked),((0,5),Empty NotChecked),((0,6),Empty NotChecked),((0,7),Empty NotChecked),((0,8),Empty NotChecked),((0,9),Empty NotChecked),((1,0),Ship NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),((1,3),Empty NotChecked),((1,4),Empty NotChecked),((1,5),Empty NotChecked),((1,6),Empty NotChecked),((1,7),Empty NotChecked),((1,8),Empty NotChecked),((1,9),Empty NotChecked),((2,0),Ship NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked),((2,3),Empty NotChecked),((2,4),Empty NotChecked),((2,5),Empty NotChecked),((2,6),Empty NotChecked),((2,7),Empty NotChecked),((2,8),Empty NotChecked),((2,9),Empty NotChecked),((3,0),Ship NotChecked),((3,1),Empty NotChecked),((3,2),Empty NotChecked),((3,3),Empty NotChecked),((3,4),Empty NotChecked),((3,5),Empty NotChecked),((3,6),Empty NotChecked),((3,7),Empty NotChecked),((3,8),Empty NotChecked),((3,9),Empty NotChecked),((4,0),Ship NotChecked),((4,1),Empty NotChecked),((4,2),Empty NotChecked),((4,3),Empty NotChecked),((4,4),Empty NotChecked),((4,5),Empty NotChecked),((4,6),Empty NotChecked),((4,7),Empty NotChecked),((4,8),Empty NotChecked),((4,9),Empty NotChecked),((5,0),Empty NotChecked),((5,1),Empty NotChecked),((5,2),Empty NotChecked),((5,3),Empty NotChecked),((5,4),Empty NotChecked),((5,5),Empty NotChecked),((5,6),Empty NotChecked),((5,7),Empty NotChecked),((5,8),Empty NotChecked),((5,9),Empty NotChecked),((6,0),Empty NotChecked),((6,1),Empty NotChecked),((6,2),Empty NotChecked),((6,3),Empty NotChecked),((6,4),Empty NotChecked),((6,5),Empty NotChecked),((6,6),Empty NotChecked),((6,7),Empty NotChecked),((6,8),Empty NotChecked),((6,9),Empty NotChecked),((7,0),Empty NotChecked),((7,1),Empty NotChecked),((7,2),Empty NotChecked),((7,3),Empty NotChecked),((7,4),Empty NotChecked),((7,5),Empty NotChecked),((7,6),Empty NotChecked),((7,7),Empty NotChecked),((7,8),Empty NotChecked),((7,9),Empty NotChecked),((8,0),Empty NotChecked),((8,1),Empty NotChecked),((8,2),Empty NotChecked),((8,3),Empty NotChecked),((8,4),Empty NotChecked),((8,5),Empty NotChecked),((8,6),Empty NotChecked),((8,7),Empty NotChecked),((8,8),Empty NotChecked),((8,9),Empty NotChecked),((9,0),Empty NotChecked),((9,1),Empty NotChecked),((9,2),Empty NotChecked),((9,3),Empty NotChecked),((9,4),Empty NotChecked),((9,5),Empty NotChecked),((9,6),Empty NotChecked),((9,7),Empty NotChecked),((9,8),Empty NotChecked),((9,9),Empty NotChecked)]) (placeShipAux initBoard (0,0) 5 Horizontal)
test7B = TestCase $ assertEqual "placeShip: increasing correct ammount of ship-cells on board" 5 (shipsCount b)
         where b = (gameBoardUser (placeShip (initGame {gameBoardAI = initBoard}) (0,0) 5 Horizontal))
test8B = TestCase $ assertEqual "placeShip edgeCase: shipsize 0" (initGame {gameBoardAI = initBoard}) (placeShip (initGame {gameBoardAI = initBoard}) (0,0) 0 Vertical)
test9B = TestCase $ assertEqual "placeShip edgeCase: start position outside of board" (initGame {gameBoardAI = initBoard}) (placeShip (initGame {gameBoardAI = initBoard}) (-1,-1) 0 Vertical)
test10B = TestCase $ assertEqual "placeShip edgeCase: end position outside of board" (initGame {gameBoardAI = initBoard}) (placeShip (initGame {gameBoardAI = initBoard}) (9,9) 5 Horizontal)

-- moving ship picture (C) --
test1C = TestCase $ assertEqual "mouseToCell: screenWidth 1440" (14,5) (mouseToCell (100, 40) ((0.0,0.0),(600.0,600.0)))
test2C = TestCase $ assertEqual "moveShip: left" (initGame {gameBoardAI = initBoard, shipsUser = [((0,0), Horizontal,5)]}) (moveShip initGame {gameBoardAI = initBoard, shipsUser = [((1,0), Horizontal,5)]} KeyLeft)
test3C = TestCase $ assertEqual "moveShip: up at upper bound" (initGame {gameBoardAI = initBoard, shipsUser = [((1,9), Horizontal,5)]}) (moveShip initGame {gameBoardAI = initBoard, shipsUser = [((1,9), Horizontal,5)]} KeyUp)
test4C = TestCase $ assertEqual "rotateShip: horizontal to vertical at upper bound" (initGame {gameBoardAI = initBoard, shipsUser = [((0,9), Vertical,5)]}) (rotateShip initGame {gameBoardAI = initBoard, shipsUser = [((0,9), Horizontal,5)]})
test5C = TestCase $ assertEqual "rotateShip: horizontal to vertical at lower bound" (initGame {gameBoardAI = initBoard, shipsUser = [((0,0), Horizontal,5)]}) (rotateShip initGame {gameBoardAI = initBoard, shipsUser = [((0,0), Horizontal,5)]})
test6C = TestCase $ assertEqual "confirmShip" (initGame {gameBoardAI = initBoard, shipsUser = [((-1,0), Horizontal,5)]}) (confirmShip initGame {gameBoardAI = initBoard, shipsUser = [((-1,0), Horizontal,5)]}) 

-- shooting user (D) --
test1D = TestCase $ assertEqual "getCell" (Empty NotChecked) (getCell initBoard (0,0))
test2D = TestCase $ assertEqual "checkCell" (array ((0,0),(9,9)) [((0,0),Empty Checked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),((0,3),Empty NotChecked),((0,4),Empty NotChecked),((0,5),Empty NotChecked),((0,6),Empty NotChecked),((0,7),Empty NotChecked),((0,8),Empty NotChecked),((0,9),Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),((1,3),Empty NotChecked),((1,4),Empty NotChecked),((1,5),Empty NotChecked),((1,6),Empty NotChecked),((1,7),Empty NotChecked),((1,8),Empty NotChecked),((1,9),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked),((2,3),Empty NotChecked),((2,4),Empty NotChecked),((2,5),Empty NotChecked),((2,6),Empty NotChecked),((2,7),Empty NotChecked),((2,8),Empty NotChecked),((2,9),Empty NotChecked),((3,0),Empty NotChecked),((3,1),Empty NotChecked),((3,2),Empty NotChecked),((3,3),Empty NotChecked),((3,4),Empty NotChecked),((3,5),Empty NotChecked),((3,6),Empty NotChecked),((3,7),Empty NotChecked),((3,8),Empty NotChecked),((3,9),Empty NotChecked),((4,0),Empty NotChecked),((4,1),Empty NotChecked),((4,2),Empty NotChecked),((4,3),Empty NotChecked),((4,4),Empty NotChecked),((4,5),Empty NotChecked),((4,6),Empty NotChecked),((4,7),Empty NotChecked),((4,8),Empty NotChecked),((4,9),Empty NotChecked),((5,0),Empty NotChecked),((5,1),Empty NotChecked),((5,2),Empty NotChecked),((5,3),Empty NotChecked),((5,4),Empty NotChecked),((5,5),Empty NotChecked),((5,6),Empty NotChecked),((5,7),Empty NotChecked),((5,8),Empty NotChecked),((5,9),Empty NotChecked),((6,0),Empty NotChecked),((6,1),Empty NotChecked),((6,2),Empty NotChecked),((6,3),Empty NotChecked),((6,4),Empty NotChecked),((6,5),Empty NotChecked),((6,6),Empty NotChecked),((6,7),Empty NotChecked),((6,8),Empty NotChecked),((6,9),Empty NotChecked),((7,0),Empty NotChecked),((7,1),Empty NotChecked),((7,2),Empty NotChecked),((7,3),Empty NotChecked),((7,4),Empty NotChecked),((7,5),Empty NotChecked),((7,6),Empty NotChecked),((7,7),Empty NotChecked),((7,8),Empty NotChecked),((7,9),Empty NotChecked),((8,0),Empty NotChecked),((8,1),Empty NotChecked),((8,2),Empty NotChecked),((8,3),Empty NotChecked),((8,4),Empty NotChecked),((8,5),Empty NotChecked),((8,6),Empty NotChecked),((8,7),Empty NotChecked),((8,8),Empty NotChecked),((8,9),Empty NotChecked),((9,0),Empty NotChecked),((9,1),Empty NotChecked),((9,2),Empty NotChecked),((9,3),Empty NotChecked),((9,4),Empty NotChecked),((9,5),Empty NotChecked),((9,6),Empty NotChecked),((9,7),Empty NotChecked),((9,8),Empty NotChecked),((9,9),Empty NotChecked)]) (checkCell initBoard (0,0))
test3D = TestCase $ assertEqual "checkCell: alredy checked cell" (checkCell (array ((0,0),(1,1)) [((0,0),Empty Checked),((0,1),Ship Checked),((1,0),Empty NotChecked),((1,1),Empty NotChecked)]) (0,0)) (checkCell (array ((0,0),(1,1)) [((0,0),Empty Checked),((0,1),Ship Checked),((1,0),Empty NotChecked),((1,1),Empty NotChecked)]) (0,0))
test4D = TestCase $ assertEqual "getState" (NotChecked) (getState initBoard (0,0))
test5D = TestCase $ assertEqual "hitShip: not hit" False (hitShip initBoard (0,0))
test6D = TestCase $ assertEqual "hitShip: hit" True (hitShip (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked)]) (0, 1))
test7D = TestCase $ assertEqual "isWithinBoard: within board" True (isWithinBoard ((900.0,0.0),(1500.0,600.0)) (200, 100))
test8D = TestCase $ assertEqual "isWithinBoard: not within board" False (isWithinBoard ((900.0,0.0),(1500.0,600.0)) (0, 0))
test9D = TestCase $ assertEqual "isChecked: not checked" False (isChecked initBoard (0,0))
test10D = TestCase $ assertEqual "isChecked: checked" True (isChecked (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),((1,0),Empty NotChecked),((1,1),Empty NotChecked)]) (0,1))
test11D = TestCase $ assertEqual "allShipsChecked: not all checked" False (allShipsChecked (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),((1,0),Empty NotChecked),((1,1), Ship NotChecked)]))
test12D = TestCase $ assertEqual "allShipsChecked: all checked" True (allShipsChecked (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),((1,0),Empty NotChecked),((1,1), Ship Checked)]))
test13D = TestCase $ assertEqual "checkWin: user win" (Just User) (checkWin (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),((1,0), Ship Checked),((1,1), Ship Checked)]) (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),((1,0), Ship Checked),((1,1), Ship Checked)]))
test14D = TestCase $ assertEqual "checkWin: AI win" (Just AI) (checkWin (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),((1,0), Ship Checked),((1,1), Ship Checked)]) (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),((1,0), Ship NotChecked),((1,1), Ship Checked)]))
test15D = TestCase $ assertEqual "checkWin no win" (Nothing) (checkWin (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship NotChecked),((1,0), Ship Checked),((1,1), Ship Checked)]) (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),((1,0), Ship NotChecked),((1,1), Ship NotChecked)]))
test16D = TestCase $ assertEqual "updateStats: user win" ((User,2),(AI,1)) (updateStats ((User, 1), (AI, 1)) (Just User))
test17D = TestCase $ assertEqual "updateStats: no win" ((User,1),(AI,1)) (updateStats ((User, 1), (AI, 1)) Nothing)
test18D = TestCase $ assertEqual "playerShoot: shooting empty cell" (Empty Checked) (getCell (gameBoardAI (playerShoot (initGame {gameBoardAI = initBoard}) (0,0))) (0,0))

-- placing AI (E) --
test1E = TestCase $ assertEqual "allCoords: n = 10" ([(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(0,8),(0,9),(1,0),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(2,0),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(2,9),(3,0),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7),(3,8),(3,9),(4,0),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(4,7),(4,8),(4,9),(5,0),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(5,7),(5,8),(5,9),(6,0),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6),(6,7),(6,8),(6,9),(7,0),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,7),(7,8),(7,9),(8,0),(8,1),(8,2),(8,3),(8,4),(8,5),(8,6),(8,7),(8,8),(8,9),(9,0),(9,1),(9,2),(9,3),(9,4),(9,5),(9,6),(9,7),(9,8),(9,9)]) (allCoords)
test2E = TestCase $ assertEqual "findValidDirectionalPlacements" ([((0,0),Horizontal),((0,1),Horizontal),((0,2),Horizontal),((1,0),Horizontal),((1,1),Horizontal),((1,2),Horizontal),((2,0),Horizontal),((2,1),Horizontal),((2,2),Horizontal)]) (findValidDirectionalPlacements initBoard [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)] 3 Horizontal)
test3E = TestCase $ assertEqual "findAllValidPlacements" ([((0,0),Horizontal),((0,1),Horizontal),((0,2),Horizontal),((0,3),Horizontal),((0,4),Horizontal),((0,5),Horizontal),((0,6),Horizontal),((0,7),Horizontal),((0,8),Horizontal),((0,9),Horizontal),((1,0),Horizontal),((1,1),Horizontal),((1,2),Horizontal),((1,3),Horizontal),((1,4),Horizontal),((1,5),Horizontal),((1,6),Horizontal),((1,7),Horizontal),((1,8),Horizontal),((1,9),Horizontal),((2,0),Horizontal),((2,1),Horizontal),((2,2),Horizontal),((2,3),Horizontal),((2,4),Horizontal),((2,5),Horizontal),((2,6),Horizontal),((2,7),Horizontal),((2,8),Horizontal),((2,9),Horizontal),((3,0),Horizontal),((3,1),Horizontal),((3,2),Horizontal),((3,3),Horizontal),((3,4),Horizontal),((3,5),Horizontal),((3,6),Horizontal),((3,7),Horizontal),((3,8),Horizontal),((3,9),Horizontal),((4,0),Horizontal),((4,1),Horizontal),((4,2),Horizontal),((4,3),Horizontal),((4,4),Horizontal),((4,5),Horizontal),((4,6),Horizontal),((4,7),Horizontal),((4,8),Horizontal),((4,9),Horizontal),((5,0),Horizontal),((5,1),Horizontal),((5,2),Horizontal),((5,3),Horizontal),((5,4),Horizontal),((5,5),Horizontal),((5,6),Horizontal),((5,7),Horizontal),((5,8),Horizontal),((5,9),Horizontal),((0,4),Vertical),((0,5),Vertical),((0,6),Vertical),((0,7),Vertical),((0,8),Vertical),((0,9),Vertical),((1,4),Vertical),((1,5),Vertical),((1,6),Vertical),((1,7),Vertical),((1,8),Vertical),((1,9),Vertical),((2,4),Vertical),((2,5),Vertical),((2,6),Vertical),((2,7),Vertical),((2,8),Vertical),((2,9),Vertical),((3,4),Vertical),((3,5),Vertical),((3,6),Vertical),((3,7),Vertical),((3,8),Vertical),((3,9),Vertical),((4,4),Vertical),((4,5),Vertical),((4,6),Vertical),((4,7),Vertical),((4,8),Vertical),((4,9),Vertical),((5,4),Vertical),((5,5),Vertical),((5,6),Vertical),((5,7),Vertical),((5,8),Vertical),((5,9),Vertical),((6,4),Vertical),((6,5),Vertical),((6,6),Vertical),((6,7),Vertical),((6,8),Vertical),((6,9),Vertical),((7,4),Vertical),((7,5),Vertical),((7,6),Vertical),((7,7),Vertical),((7,8),Vertical),((7,9),Vertical),((8,4),Vertical),((8,5),Vertical),((8,6),Vertical),((8,7),Vertical),((8,8),Vertical),((8,9),Vertical),((9,4),Vertical),((9,5),Vertical),((9,6),Vertical),((9,7),Vertical),((9,8),Vertical),((9,9),Vertical)]) (findAllValidPlacements initBoard 5)
test4E = TestCase $ assertEqual "randomElement" 7 (fst $ randomElement [1,2,3,4,5,6,7,8] (mkStdGen 10))
test5E = TestCase $ assertEqual "placeShipAI ship length 5 " 3 (shipsCount $ fst $ placeShipAI (mkStdGen 10) initBoard 3 (findAllValidPlacements initBoard 3))
test6E = TestCase $ assertEqual "placeMultipleShipsAI without ships" initBoard (fst $ placeMultipleShipsAI (mkStdGen 10) initBoard [])
test7E = TestCase $ assertEqual "placeMultipleShipsAI: correct total ship-cells" 17 (shipsCount b)
         where b = fst $ placeMultipleShipsAI (mkStdGen 10) initBoard initShips

-- shooting AI (F) --
test1F = TestCase $ assertEqual "getCol" 2 (getCol ((2,3),Empty NotChecked))
test2F = TestCase $ assertEqual "getRow" 3 (getRow ((2,3),Empty NotChecked))
test3F = TestCase $ assertEqual "aiShootList" ([((0,0),Empty NotChecked),((0,1),Empty Checked),((0,2),Empty NotChecked),((1,0),Ship Checked),((1,1),Ship Checked),((1,2),Ship NotChecked),((2,0),Empty NotChecked),((2,1),Empty Checked),((2,2),Empty NotChecked)]) (aiShootList (array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty Checked),((0,2),Empty NotChecked),((1,0),Ship Checked),((1,1),Ship Checked),((1,2),Ship NotChecked),((2,0),Empty NotChecked),((2,1),Empty Checked),((2,2),Empty NotChecked)]))
test4F = TestCase $ assertEqual "aiPrio" ([((2,1),Empty Checked),((1,2),Ship NotChecked),((1,0),Ship Checked),((0,1),Empty Checked),((0,0),Empty NotChecked),((0,2),Empty NotChecked),((1,1),Ship Checked),((2,0),Empty NotChecked),((2,2),Empty NotChecked)]) (aiPrio [((0,0),Empty NotChecked),((0,1),Empty Checked),((0,2),Empty NotChecked),((1,0),Ship Checked),((1,1),Ship Checked),((1,2),Ship NotChecked),((2,0),Empty NotChecked),((2,1),Empty Checked),((2,2),Empty NotChecked)] [])
test5F = TestCase $ assertEqual "filterShootList" ([((1,2),Ship NotChecked),((0,2),Empty NotChecked),((0,0),Empty NotChecked),((2,2),Empty NotChecked),((2,0),Empty NotChecked)]) (fst $ filterShootList (array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty Checked),((0,2),Empty NotChecked),((1,0),Ship Checked),((1,1),Ship Checked),((1,2),Ship NotChecked),((2,0),Empty NotChecked),((2,1),Empty Checked),((2,2),Empty NotChecked)]) (mkStdGen 10))
test6F = TestCase $ assertEqual "removeChecked" ([((0,0),Empty NotChecked),((0,2),Empty NotChecked),((1,2),Ship NotChecked),((2,0),Empty NotChecked),((2,2),Empty NotChecked)]) (removeChecked [((0,0),Empty NotChecked),((0,1),Empty Checked),((0,2),Empty NotChecked),((1,0),Ship Checked),((1,1),Ship Checked),((1,2),Ship NotChecked),((2,0),Empty NotChecked),((2,1),Empty Checked),((2,2),Empty NotChecked)])
test7F = TestCase $ assertEqual "updateStack: empty stack" ([((0,0),Empty NotChecked)]) (updateStack [] [((0,0),Empty NotChecked),((1,2),Ship NotChecked)])
test8F = TestCase $ assertEqual "updateStack: non-empty stack" ([((0,0),Empty NotChecked)]) (updateStack [((0,0),Empty NotChecked)] [((1,2),Ship NotChecked)])
test9F = TestCase $ assertEqual "updateStack: empty Stack and ShootList" [] (updateStack [] [])
test10F = TestCase $ assertEqual "cohesiveCells" ([((1,1),Ship Checked),((0,0),Empty NotChecked),((2,0),Empty NotChecked)]) (cohesiveCells (array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty Checked),((0,2),Empty NotChecked),((1,0),Ship Checked),((1,1),Ship Checked),((1,2),Ship NotChecked),((2,0),Empty NotChecked),((2,1),Empty Checked),((2,2),Empty NotChecked)]) ((1,0),Ship Checked))
test11F = TestCase $ assertEqual "isShip: not checked ship" True (isShip ((1,1),Ship NotChecked))
test12F = TestCase $ assertEqual "isShip: already checked ship" False (isShip ((9,0),Ship Checked))
test13F = TestCase $ assertEqual "aiShootAux" (array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),((1,0),Empty Checked),((1,1),Empty NotChecked)],[]) (aiShootAux (array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),((1,0),Empty NotChecked),((1,1),Empty NotChecked)],[((1,0),Empty NotChecked)]))
test14F = TestCase $ assertEqual "aiShoot: empty stack" (array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),((1,0),Empty Checked),((1,1),Empty NotChecked)],[]) (fst $ aiShoot (array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),((1,0),Empty NotChecked),((1,1),Empty NotChecked)],[((1,0),Empty NotChecked)]) (mkStdGen 10))
test15F = TestCase $ assertEqual "aiShoot: non-empty stack" ((array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),((1,0),Empty Checked),((1,1),Empty NotChecked)],[])) (fst $ aiShoot (array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),((1,0),Empty NotChecked),((1,1),Empty NotChecked)],[((1,0),Empty NotChecked)]) (mkStdGen 10))
test16F = TestCase $ assertEqual "aiShoot: increasing shot cells" 1  (cellCount (Empty Checked) b)
          where ((b, _),_) = aiShoot (initBoard, []) (mkStdGen 10)
test17F = TestCase $ assertEqual "aiShoot: shooting ship" 1 (cellCount (Ship Checked) b)
          where ((b, _),_)= aiShoot (shipBoard, []) (mkStdGen 10)
                shipBoard = array ((0, 0), (9,9)) $ zip (range ((0, 0), (9,9))) (repeat $ Ship NotChecked)

-- run --
testsA = TestList [  test1A
                   , test2A
                   , test3A
                   , test4A
                   , test5A
                   , test6A
                   , test7A
                    ]

testsB = TestList [  test1B 
                   , test2B 
                   , test3B 
                   , test4B 
                   , test5B 
                   , test6B 
                   , test7B   
                   , test8B 
                   , test9B 
                   , test10B
                    ]

testsC = TestList [  test1C
                   , test2C
                   , test3C
                   , test4C
                   , test5C
                   , test6C
                    ]

testsD = TestList [  test1D 
                   , test2D 
                   , test3D 
                   , test4D 
                   , test5D 
                   , test6D 
                   , test7D 
                   , test8D 
                   , test9D 
                   , test10D
                   , test11D
                   , test12D
                   , test13D
                   , test14D
                   , test15D
                   , test16D
                   , test17D
                   , test18D
                    ]

testsE = TestList [  test1E
                   , test2E
                   , test3E
                   , test4E
                   , test5E
                   , test6E
                   , test7E
                    ]

testsF = TestList [  test1F 
                   , test2F 
                   , test3F 
                   , test4F 
                   , test5F 
                   , test6F 
                   , test7F 
                   , test8F 
                   , test9F 
                   , test10F
                   , test11F
                   , test12F
                   , test13F
                   , test14F
                   , test15F
                   , test16F
                   , test17F
                    ]

tests = TestList [   test1A
                   , test2A
                   , test3A
                   , test4A
                   , test5A
                   , test6A
                   , test7A
                   , test1B 
                   , test2B 
                   , test3B 
                   , test4B 
                   , test5B 
                   , test6B 
                   , test7B 
                   , test8B 
                   , test9B 
                   , test10B
                   , test1C
                   , test2C
                   , test3C
                   , test4C
                   , test5C
                   , test6C
                   , test1D 
                   , test2D 
                   , test3D 
                   , test4D 
                   , test5D 
                   , test6D 
                   , test7D 
                   , test8D 
                   , test9D 
                   , test10D
                   , test11D
                   , test12D
                   , test13D
                   , test14D
                   , test15D
                   , test16D
                   , test17D
                   , test18D
                   , test1E
                   , test2E
                   , test3E
                   , test4E
                   , test5E
                   , test6E
                   , test7E
                   , test1F 
                   , test2F 
                   , test3F 
                   , test4F 
                   , test5F 
                   , test6F 
                   , test7F 
                   , test8F 
                   , test9F 
                   , test10F
                   , test11F
                   , test12F
                   , test13F
                   , test14F
                   , test15F
                   , test16F
                   , test17F
                    ]

runtestsA = runTestTT $ testsA
runtestsB = runTestTT $ testsB
runtestsC = runTestTT $ testsC
runtestsD = runTestTT $ testsD
runtestsE = runTestTT $ testsE
runtestsF = runTestTT $ testsF

runtests = runTestTT $ tests
