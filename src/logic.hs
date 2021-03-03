module Logic where
import Shuffle
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Data.Array
import Game
import Data.List
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

{- surroundingCells board coord shipsize direction
   Get all cell coordinates that surrounds a ship starting at coord with shipsize and direction
   RETURNS: A list containing cell coordinates that a ship starting at coord with shipsize and direction
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


{- followPlacementRules board coord shipsize direction
   Check if ship placement follows the games placement rules
   RETURNS: True if placement of a ship with shipsize and direction starting at coord,
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

{- validShipPlacement coord shipsize direction
   Check if ship placement is valid
   RETURNS: True if placement of a ship with shipsize and direction starting at coord, is valid, else False.
   EXAMPLES: validShipPlacement (array ((0,0),(3,3)) [((0,0),Empty NotChecked),((0,1),Ship NotChecked),((0,2),Ship NotChecked),((0,3),
                                                      Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),
                                                     ((1,3),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked),
                                                     ((2,3),Empty NotChecked),((3,0),Empty NotChecked),((3,1),Empty NotChecked),((3,2),Empty NotChecked),
                                                     ((3,3),Empty NotChecked)])
                                                     (0,0) 3 Horizontal == False
             validShipPlacement (array ((0,0),(3,3)) [((0,0),Empty NotChecked),((0,1),Ship NotChecked),((0,2),Ship NotChecked),((0,3)
                                                     ,Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),
                                                     ((1,3),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked),
                                                     ((2,3),Empty NotChecked),((3,0),Empty NotChecked),((3,1),Empty NotChecked),((3,2),Empty NotChecked),
                                                     ((3,3),Empty NotChecked)])
                                                     (3,3) 4 Vertical == True
-}

validShipPlacement :: Board ->  CellCoord -> ShipSize -> Direction -> Bool
validShipPlacement b (c, r) s d = validCoordinates (endCoordinates (c, r) s d)
                                  && validCoordinates (c, r)  
                                  && followPlacementRules b (c,r) s d

{- placeShipAux board coord shipsize direction
   Places a ship with shipsize and direction, on board starting at coord
   VARIANT: Placement of ship is valid
   RETURNS: updated board where ship wtih shipsize and direction, starting at coord, have been placed.
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
-- VARIANT: shipsize
placeShipAux b _ 0 _= b
placeShipAux b (c, r) s Vertical = placeShipAux (b // [((c, r), Ship NotChecked)]) (c, r - 1) (s - 1) Vertical
placeShipAux b (c, r) s Horizontal = placeShipAux (b // [((c, r), Ship NotChecked)]) (c + 1, r) (s - 1) Horizontal

{- placeShip game coord shipsize direction
   Places a ship with shipsize and direction, on board starting at coord
   PRE: Placement of ship is valid
   RETURNS: If placement is valid then it returns game where ship with shipsize, direction,
            starting at coord have been placed in gameBoardUser, otherwise game unchanged.
   EXAMPLES: 
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
    EXAMPLES: mouseToCell (100, 40) ((0.0,0.0),(600.0,600.0)) == (5,2)
              mouseToCell (1000, 40) ((900.0,0.0),(1500.0,600.0)) == (5,2)
-}

mouseToCell :: ScreenCoord -> BoardPos -> CellCoord
mouseToCell (x, y) boardPos@((x1,y1),(x2,y2)) =  (floor ((x - x1 + boardWidth + screenDivider * 0.5) / cellWidth), floor ((y - y1 + boardHeight  * 0.5) / cellHeight ))


{- moveShip game key
    updates the first placing ship's cellCoordinates according to key input
    RETURNS: If valid position then head of shipsUser game with new coordinates according to key, else game.

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
    changes the first placing ship's direction
    PRE: shipsUser in game not empty
    RETURNS: If opposite direction is valid then head of shipsUser game with opposite direction, else game.

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
    RETURNS: if coord == unchecked then board with checked coord, else board.
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
    RETURNS: True if coord == checked, else False.
    EXAMPLES: isChecked (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),
                        ((1,0),Empty NotChecked),((1,1),Empty NotChecked)]) (0,0) == False
              isChecked (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),
                        ((1,0),Empty NotChecked),((1,1),Empty NotChecked)]) (0,1) == True
-}

isChecked :: Board -> CellCoord -> Bool
isChecked b coord = s == Checked
        where s = getState b coord

{- allShipsChecked board 
    checks if board has won
    RETURNS: True if all ships are checked on board, else false
    EXAMPLES: allShipsChecked (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),
                              ((1,0),Empty NotChecked),((1,1), Ship NotChecked)]) == False
              allShipsChecked (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),
                              ((1,0),Empty NotChecked),((1,1), Ship Checked)]) == True
-}

allShipsChecked :: Board -> Bool
allShipsChecked b = not $ any (\cell -> cell == Ship NotChecked) b

{- checkWin boardUser boardAI 
    checks if user or AI won
    RETURNS: the player who won first
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
    EXAMPLES: updateStats ((User, 1), (AI, 1)) (Just User) = ((User, 2), (AI, 1))
              updateStats ((User, 1), (AI, 1)) (Just AI)   = ((User, 1), (AI, 2))
              updateStats ((User, 1), (AI, 1)) Nothing     = ((User, 1), (AI, 1))
-}

updateStats :: Stats -> Maybe Player -> Stats
updateStats s@((user, n1), (ai, n2)) player = case player of 
                                         Just User -> ((user, n1 + 1), (ai, n2))
                                         Just AI   -> ((user, n1), (ai, n2 + 1))
                                         _         -> s

{- playerShoot game coord 
    shoots the coord for user. Calls AI to shoot. Updates game accordingly
    RETURNS: game after user and AI have shot. 
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
    EXAMPLES: allCoords == [(0,0),(0,1),(0,2),(0,3),(1,0),(1,1),(1,2),(1,3),(2,0),(2,1),(2,2),(2,3),(3,0),(3,1),(3,2),(3,3)]
                where n = 4
-}

allCoords :: [CellCoord]
allCoords = [(c, r) | c <- [0..n-1], r <- [0..n-1]]

{- findValidDirectionalPlacements board coords shipsize direction
   finds possible positions of a ship in a certain direction
   RETURNS: all possible valid coords of ship with direction on board
   EXAMPLES: findValidDirectionalPlacements array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),
                                                  ((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),
                                                  ((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked)]
                                                  [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)] 3 Horizontal
                                                  == [((0,0),Horizontal),((0,1),Horizontal),((0,2),Horizontal)]
             findValidDirectionalPlacements array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),
                                                  ((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),
                                                  ((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked)]
                                                  [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)] 2 Vertical
                                                  == [((0,1),Vertical),((0,2),Vertical),((1,1),Vertical),((1,2),Vertical),((2,1),Vertical),((2,2),Vertical)]
-}

findValidDirectionalPlacements :: Board -> [CellCoord] -> ShipSize ->  Direction -> [(CellCoord, Direction)]
findValidDirectionalPlacements b coords s d = map (\coord -> (coord, d)) $ filter (\coord -> validShipPlacement b coord s d) coords
                 
{- findAllValidPlacements board ship 
    finds all valid placments of ship on board
    RETURNS: all valid placements of ship on board
    EXAMPLES: findAllValidPlacements (array ((0,0),(2,2)) [((0,0),Empty NotChecked),
              ((0,1),Empty NotChecked),((0,2),Empty NotChecked),((1,0),Empty NotChecked),
              ((1,1),Empty NotChecked),((1,2),Empty NotChecked),((2,0),Empty NotChecked),
              ((2,1),Empty NotChecked),((2,2),Empty NotChecked)]) 3
              == [((0,0),Horizontal),((0,1),Horizontal),((0,2),Horizontal),
                 ((0,2),Vertical),((1,2),Vertical),((2,2),Vertical)]
-}

findAllValidPlacements :: Board -> ShipSize -> [(CellCoord, Direction)]
findAllValidPlacements b s = findValidDirectionalPlacements b allCoords s Horizontal ++ findValidDirectionalPlacements b allCoords s Vertical 

{- randomElement list gen
    randomly picks an element of list
    RETURNS: (randomly generated element of list with gen, new seed generated with gen)
    EXAMPLES: randomElement [1,2,3,4,5,6,7,8] (mkStdGen 10) == (7,StdGen {unStdGen = SMGen 7847668597276082904 614480483733483467})
              randomElement [1,2,3,4,5,6,7,8] (mkStdGen 11) == (4,StdGen {unStdGen = SMGen 4664641791676752737 5833679380957638813})
              randomElement ['a','b','c','d','e'] (mkStdGen 10) == ('d',StdGen {unStdGen = SMGen 9076629564743049838 614480483733483467})
-}
randomElement :: [a] -> StdGen -> (a, StdGen)
randomElement list gen = (list !! randomInt, newGen)
                     where range = (0, length list - 1)
                           (randomInt, newGen) = randomR range gen

{- placeShipAI gen board ship placements
       updates board with a random placement of ship
       RETURNS: (board with ship integrated randomly with placements and gen, new seed generated with gen)
       EXAMPLES: placeShipAI (mkStdGen 10) b 3 (findAllValidPlacements b 3)
                    where b = array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),
                    ((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),((2,0),Empty NotChecked),
                    ((2,1),Empty NotChecked),((2,2),Empty NotChecked)]
                    == (array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),
                    ((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),((2,0),Ship NotChecked),
                    ((2,1),Ship NotChecked),((2,2),Ship NotChecked)],StdGen {unStdGen = SMGen 8462149081009566371 614480483733483467})
-}

placeShipAI :: StdGen -> Board -> ShipSize -> [(CellCoord, Direction)] -> (Board, StdGen)
placeShipAI gen b s placements = (placeShipAux b coord s d, newGen)
                             where ((coord , d), newGen) = randomElement placements gen


{- placeMultipleShipsAI gen board ships
       places the ships on random places on the board
       RETURNS: (ships integrated randomly on board with gen, new seed generated with gen)
       EXAMPLES: placeMultipleShipsAI (mkStdGen 10) (array ((0,0),(2,2)) [((0,0),Empty NotChecked),
                 ((0,1),Empty NotChecked),((0,2),Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),
                 ((1,2),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked)]) 
                 [((1,2), Horizontal, 2),((0,0), Vertical, 2)]
                 == (array ((0,0),(2,2)) [((0,0),Ship NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),
                 ((1,0),Ship NotChecked),((1,1),Empty NotChecked),((1,2),Ship NotChecked),((2,0),Empty NotChecked),
                 ((2,1),Empty NotChecked),((2,2),Ship NotChecked)],StdGen {unStdGen = SMGen 10305590532210016772 614480483733483467})
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
   EXAMPLES: aiPrio [((0,0),Empty NotChecked),
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
   EXAMPLES: filterShootList (array ((0,0),(2,2)) [((0,0),Empty NotChecked),
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

{- aiShootAux (board,stack) sl
   checks first cell in stack
   PRE: Stack is not empty, ShootList is not empty
   RETURNS: (board,stack) where first cell in stack is checked in board
   EXAMPLES: aiShootAux (array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),
                        ((1,0),Empty NotChecked),((1,1),Empty NotChecked)],[((1,0),Empty NotChecked)]) 
                        [((0,0),Ship NotChecked),((1,1),Empty NotChecked)] 
                        == (array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),
                           ((1,0),Empty Checked),((1,1),Empty NotChecked)],[])
-}
aiShootAux :: (Board,Stack) -> ShootList -> (Board,Stack)
aiShootAux (b, s@(coord,cell):st)  l | isShip s = (checkCell b coord, removeChecked $ nub (cohesiveCells b s ++ st))
                                     | otherwise = (checkCell b coord, st)

{- aiShoot (board,stack) gen
   checks first cell in stack
   PRE: ShootList is not empty
   RETURNS: ((board,stack),gen) where first cell in stack is checked in board
   EXAMPLES: aiShoot (array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),
                     ((1,0),Empty NotChecked),((1,1),Empty NotChecked)],
                     [((1,0),Empty NotChecked)]) (mkStdGen 10) 
                     == ((array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),
                        ((1,0),Empty Checked),((1,1),Empty NotChecked)],[]),StdGen 
                        {unStdGen = SMGen 9076629564743049838 614480483733483467})
             aiShoot (array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),
                     ((1,0),Empty NotChecked),((1,1),Empty NotChecked)],
                     []) (mkStdGen 10) 
                     == ((array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),
                        ((1,0),Empty Checked),((1,1),Empty NotChecked)],[]),StdGen 
                        {unStdGen = SMGen 9076629564743049838 614480483733483467})
-}
aiShoot :: (Board,Stack) -> StdGen -> ((Board, Stack), StdGen)
aiShoot (b,s) gen = (aiShootAux (b,removeChecked $ updateStack s newList) newList,newGen)
                     where (newList, newGen) = filterShootList b gen

--------------------- EventHandler --------------------------------


{- eventHandler event game
    handles all input from user
    RETURNS: game with possible modifications decided by event
-}

eventHandler :: Event -> Game -> Game
eventHandler (EventKey (SpecialKey KeyEnter) Down _ _) game =
     case gameStage game of 
         Placing User -> confirmShip game
         _            -> game

eventHandler (EventKey (SpecialKey key) Down _ _) game =
    case gameStage game of 
        Placing User -> moveShip game key
        _            -> game

eventHandler (EventKey (Char 'r') Down _ _) game =             
    case gameStage game of 
         Placing User -> rotateShip game
         _            -> game

eventHandler (EventKey (MouseButton LeftButton) Up _ mousePos) game =

    case (winner game, gameStage game) of
         (Nothing, Shooting User) -> playerShoot game {shootAnimation = (hitShip (gameBoardAI game) coord,startRadius, 
                                                       mousePos, end, startDerivative, performAnimation)} coord 
                                                where (_,_,_, end, _, _) = shootAnimation game
                                                      coord = mouseToCell mousePos boardAIPos
                                                      performAnimation = isWithinBoard boardAIPos mousePos 
                                                                         && getState (gameBoardAI game) coord == NotChecked
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
 


test9 = TestCase $ assertEqual "validCoordinates: coordinates out of bounds" False (validCoordinates (2,11))
test10 = TestCase $ assertEqual "endCoordinates: vertical placement" (5,4) (endCoordinates (5,6) 3 Vertical)
test11 = TestCase $ assertEqual "surroundingCells: horizontal placement" [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(0,0),(1,0),(2,0),(3,0),(4,0),(0,1),(1,1),(2,1),(3,1),(4,1),(-1,0),(5,0)] (surroundingCells initBoard (0,0) 5 Horizontal)
test12 = TestCase $ assertEqual "followPlacementRules: vertical placement" True (followPlacementRules initBoard (5,6) 3 Vertical)
test13 = TestCase $ assertEqual "validShipPlacement: ship out of bounds" False (validShipPlacement initBoard (8,0) 3 Vertical)
test14 = TestCase $ assertEqual "placeShipAux: place a horizontal ship" (array ((0,0),(9,9)) [((0,0),Ship NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),((0,3),Empty NotChecked),((0,4),Empty NotChecked),((0,5),Empty NotChecked),((0,6),Empty NotChecked),((0,7),Empty NotChecked),((0,8),Empty NotChecked),((0,9),Empty NotChecked),((1,0),Ship NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),((1,3),Empty NotChecked),((1,4),Empty NotChecked),((1,5),Empty NotChecked),((1,6),Empty NotChecked),((1,7),Empty NotChecked),((1,8),Empty NotChecked),((1,9),Empty NotChecked),((2,0),Ship NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked),((2,3),Empty NotChecked),((2,4),Empty NotChecked),((2,5),Empty NotChecked),((2,6),Empty NotChecked),((2,7),Empty NotChecked),((2,8),Empty NotChecked),((2,9),Empty NotChecked),((3,0),Ship NotChecked),((3,1),Empty NotChecked),((3,2),Empty NotChecked),((3,3),Empty NotChecked),((3,4),Empty NotChecked),((3,5),Empty NotChecked),((3,6),Empty NotChecked),((3,7),Empty NotChecked),((3,8),Empty NotChecked),((3,9),Empty NotChecked),((4,0),Ship NotChecked),((4,1),Empty NotChecked),((4,2),Empty NotChecked),((4,3),Empty NotChecked),((4,4),Empty NotChecked),((4,5),Empty NotChecked),((4,6),Empty NotChecked),((4,7),Empty NotChecked),((4,8),Empty NotChecked),((4,9),Empty NotChecked),((5,0),Empty NotChecked),((5,1),Empty NotChecked),((5,2),Empty NotChecked),((5,3),Empty NotChecked),((5,4),Empty NotChecked),((5,5),Empty NotChecked),((5,6),Empty NotChecked),((5,7),Empty NotChecked),((5,8),Empty NotChecked),((5,9),Empty NotChecked),((6,0),Empty NotChecked),((6,1),Empty NotChecked),((6,2),Empty NotChecked),((6,3),Empty NotChecked),((6,4),Empty NotChecked),((6,5),Empty NotChecked),((6,6),Empty NotChecked),((6,7),Empty NotChecked),((6,8),Empty NotChecked),((6,9),Empty NotChecked),((7,0),Empty NotChecked),((7,1),Empty NotChecked),((7,2),Empty NotChecked),((7,3),Empty NotChecked),((7,4),Empty NotChecked),((7,5),Empty NotChecked),((7,6),Empty NotChecked),((7,7),Empty NotChecked),((7,8),Empty NotChecked),((7,9),Empty NotChecked),((8,0),Empty NotChecked),((8,1),Empty NotChecked),((8,2),Empty NotChecked),((8,3),Empty NotChecked),((8,4),Empty NotChecked),((8,5),Empty NotChecked),((8,6),Empty NotChecked),((8,7),Empty NotChecked),((8,8),Empty NotChecked),((8,9),Empty NotChecked),((9,0),Empty NotChecked),((9,1),Empty NotChecked),((9,2),Empty NotChecked),((9,3),Empty NotChecked),((9,4),Empty NotChecked),((9,5),Empty NotChecked),((9,6),Empty NotChecked),((9,7),Empty NotChecked),((9,8),Empty NotChecked),((9,9),Empty NotChecked)]) (placeShipAux initBoard (0,0) 5 Horizontal)
test15 = TestCase $ assertEqual "mouseToCell: screenWidth 1440" (14,5) (mouseToCell (100, 40) ((0.0,0.0),(600.0,600.0)))
test16 = TestCase $ assertEqual "getCell" (Empty NotChecked) (getCell initBoard (0,0))
test17 = TestCase $ assertEqual "checkCell" (array ((0,0),(9,9)) [((0,0),Empty Checked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),((0,3),Empty NotChecked),((0,4),Empty NotChecked),((0,5),Empty NotChecked),((0,6),Empty NotChecked),((0,7),Empty NotChecked),((0,8),Empty NotChecked),((0,9),Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),((1,3),Empty NotChecked),((1,4),Empty NotChecked),((1,5),Empty NotChecked),((1,6),Empty NotChecked),((1,7),Empty NotChecked),((1,8),Empty NotChecked),((1,9),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked),((2,3),Empty NotChecked),((2,4),Empty NotChecked),((2,5),Empty NotChecked),((2,6),Empty NotChecked),((2,7),Empty NotChecked),((2,8),Empty NotChecked),((2,9),Empty NotChecked),((3,0),Empty NotChecked),((3,1),Empty NotChecked),((3,2),Empty NotChecked),((3,3),Empty NotChecked),((3,4),Empty NotChecked),((3,5),Empty NotChecked),((3,6),Empty NotChecked),((3,7),Empty NotChecked),((3,8),Empty NotChecked),((3,9),Empty NotChecked),((4,0),Empty NotChecked),((4,1),Empty NotChecked),((4,2),Empty NotChecked),((4,3),Empty NotChecked),((4,4),Empty NotChecked),((4,5),Empty NotChecked),((4,6),Empty NotChecked),((4,7),Empty NotChecked),((4,8),Empty NotChecked),((4,9),Empty NotChecked),((5,0),Empty NotChecked),((5,1),Empty NotChecked),((5,2),Empty NotChecked),((5,3),Empty NotChecked),((5,4),Empty NotChecked),((5,5),Empty NotChecked),((5,6),Empty NotChecked),((5,7),Empty NotChecked),((5,8),Empty NotChecked),((5,9),Empty NotChecked),((6,0),Empty NotChecked),((6,1),Empty NotChecked),((6,2),Empty NotChecked),((6,3),Empty NotChecked),((6,4),Empty NotChecked),((6,5),Empty NotChecked),((6,6),Empty NotChecked),((6,7),Empty NotChecked),((6,8),Empty NotChecked),((6,9),Empty NotChecked),((7,0),Empty NotChecked),((7,1),Empty NotChecked),((7,2),Empty NotChecked),((7,3),Empty NotChecked),((7,4),Empty NotChecked),((7,5),Empty NotChecked),((7,6),Empty NotChecked),((7,7),Empty NotChecked),((7,8),Empty NotChecked),((7,9),Empty NotChecked),((8,0),Empty NotChecked),((8,1),Empty NotChecked),((8,2),Empty NotChecked),((8,3),Empty NotChecked),((8,4),Empty NotChecked),((8,5),Empty NotChecked),((8,6),Empty NotChecked),((8,7),Empty NotChecked),((8,8),Empty NotChecked),((8,9),Empty NotChecked),((9,0),Empty NotChecked),((9,1),Empty NotChecked),((9,2),Empty NotChecked),((9,3),Empty NotChecked),((9,4),Empty NotChecked),((9,5),Empty NotChecked),((9,6),Empty NotChecked),((9,7),Empty NotChecked),((9,8),Empty NotChecked),((9,9),Empty NotChecked)]) (checkCell initBoard (0,0))
test18 = TestCase $ assertEqual "getState" (NotChecked) (getState initBoard (0,0))
test19 = TestCase $ assertEqual "hitShip" False (hitShip initBoard (0,0))
test20 = TestCase $ assertEqual "isWithinBoard" True (isWithinBoard ((900.0,0.0),(1500.0,600.0)) (200, 100))
test21 = TestCase $ assertEqual "isChecked" False (isChecked initBoard (0,0))
test22 = TestCase $ assertEqual "checkWin: user win" (Just User) (checkWin (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),((1,0), Ship Checked),((1,1), Ship Checked)]) (array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Ship Checked),((1,0), Ship Checked),((1,1), Ship Checked)]))
test23 = TestCase $ assertEqual "updateStats: user win" ((User,2),(AI,1)) (updateStats ((User, 1), (AI, 1)) (Just User))
test24 = TestCase $ assertEqual "allCoords: n = 10" ([(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(0,8),(0,9),(1,0),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(2,0),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(2,9),(3,0),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7),(3,8),(3,9),(4,0),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(4,7),(4,8),(4,9),(5,0),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(5,7),(5,8),(5,9),(6,0),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6),(6,7),(6,8),(6,9),(7,0),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,7),(7,8),(7,9),(8,0),(8,1),(8,2),(8,3),(8,4),(8,5),(8,6),(8,7),(8,8),(8,9),(9,0),(9,1),(9,2),(9,3),(9,4),(9,5),(9,6),(9,7),(9,8),(9,9)]) (allCoords)
test25 = TestCase $ assertEqual "findValidDirectionalPlacements" ([((0,0),Horizontal),((0,1),Horizontal),((0,2),Horizontal),((1,0),Horizontal),((1,1),Horizontal),((1,2),Horizontal),((2,0),Horizontal),((2,1),Horizontal),((2,2),Horizontal)]) (findValidDirectionalPlacements initBoard [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)] 3 Horizontal)
test26 = TestCase $ assertEqual "findAllValidPlacements" ([((0,0),Horizontal),((0,1),Horizontal),((0,2),Horizontal),((0,3),Horizontal),((0,4),Horizontal),((0,5),Horizontal),((0,6),Horizontal),((0,7),Horizontal),((0,8),Horizontal),((0,9),Horizontal),((1,0),Horizontal),((1,1),Horizontal),((1,2),Horizontal),((1,3),Horizontal),((1,4),Horizontal),((1,5),Horizontal),((1,6),Horizontal),((1,7),Horizontal),((1,8),Horizontal),((1,9),Horizontal),((2,0),Horizontal),((2,1),Horizontal),((2,2),Horizontal),((2,3),Horizontal),((2,4),Horizontal),((2,5),Horizontal),((2,6),Horizontal),((2,7),Horizontal),((2,8),Horizontal),((2,9),Horizontal),((3,0),Horizontal),((3,1),Horizontal),((3,2),Horizontal),((3,3),Horizontal),((3,4),Horizontal),((3,5),Horizontal),((3,6),Horizontal),((3,7),Horizontal),((3,8),Horizontal),((3,9),Horizontal),((4,0),Horizontal),((4,1),Horizontal),((4,2),Horizontal),((4,3),Horizontal),((4,4),Horizontal),((4,5),Horizontal),((4,6),Horizontal),((4,7),Horizontal),((4,8),Horizontal),((4,9),Horizontal),((5,0),Horizontal),((5,1),Horizontal),((5,2),Horizontal),((5,3),Horizontal),((5,4),Horizontal),((5,5),Horizontal),((5,6),Horizontal),((5,7),Horizontal),((5,8),Horizontal),((5,9),Horizontal),((0,4),Vertical),((0,5),Vertical),((0,6),Vertical),((0,7),Vertical),((0,8),Vertical),((0,9),Vertical),((1,4),Vertical),((1,5),Vertical),((1,6),Vertical),((1,7),Vertical),((1,8),Vertical),((1,9),Vertical),((2,4),Vertical),((2,5),Vertical),((2,6),Vertical),((2,7),Vertical),((2,8),Vertical),((2,9),Vertical),((3,4),Vertical),((3,5),Vertical),((3,6),Vertical),((3,7),Vertical),((3,8),Vertical),((3,9),Vertical),((4,4),Vertical),((4,5),Vertical),((4,6),Vertical),((4,7),Vertical),((4,8),Vertical),((4,9),Vertical),((5,4),Vertical),((5,5),Vertical),((5,6),Vertical),((5,7),Vertical),((5,8),Vertical),((5,9),Vertical),((6,4),Vertical),((6,5),Vertical),((6,6),Vertical),((6,7),Vertical),((6,8),Vertical),((6,9),Vertical),((7,4),Vertical),((7,5),Vertical),((7,6),Vertical),((7,7),Vertical),((7,8),Vertical),((7,9),Vertical),((8,4),Vertical),((8,5),Vertical),((8,6),Vertical),((8,7),Vertical),((8,8),Vertical),((8,9),Vertical),((9,4),Vertical),((9,5),Vertical),((9,6),Vertical),((9,7),Vertical),((9,8),Vertical),((9,9),Vertical)]) (findAllValidPlacements initBoard 5)
test27 = TestCase $ assertEqual "randomElement" (7,StdGen {unStdGen = SMGen 7847668597276082904 614480483733483467}) (randomElement [1,2,3,4,5,6,7,8] (mkStdGen 10))
test28 = TestCase $ assertEqual "placeShipAI" (array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),((2,0),Ship NotChecked),((2,1),Ship NotChecked),((2,2),Ship NotChecked)],StdGen {unStdGen = SMGen 8462149081009566371 614480483733483467}) (placeShipAI (mkStdGen 10) b 3 (findAllValidPlacements b 3))
         where b = (array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked)])
test29 = TestCase $ assertEqual "placeMultipleShipsAI" (array ((0,0),(2,2)) [((0,0),Ship NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),((1,0),Ship NotChecked),((1,1),Empty NotChecked),((1,2),Ship NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Ship NotChecked)],StdGen {unStdGen = SMGen 10305590532210016772 614480483733483467}) (placeMultipleShipsAI (mkStdGen 10) (array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked)]) [((1,2), Horizontal, 2),((0,0), Vertical, 2)])
test30 = TestCase $ assertEqual "getCol" 2 (getCol ((2,3),Empty NotChecked))
test31 = TestCase $ assertEqual "getRow" 3 (getRow ((2,3),Empty NotChecked))
test32 = TestCase $ assertEqual "aiShootList" ([((0,0),Empty NotChecked),((0,1),Empty Checked),((0,2),Empty NotChecked),((1,0),Ship Checked),((1,1),Ship Checked),((1,2),Ship NotChecked),((2,0),Empty NotChecked),((2,1),Empty Checked),((2,2),Empty NotChecked)]) (aiShootList (array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty Checked),((0,2),Empty NotChecked),((1,0),Ship Checked),((1,1),Ship Checked),((1,2),Ship NotChecked),((2,0),Empty NotChecked),((2,1),Empty Checked),((2,2),Empty NotChecked)]))
test33 = TestCase $ assertEqual "aiPrio" ([((2,1),Empty Checked),((1,2),Ship NotChecked),((1,0),Ship Checked),((0,1),Empty Checked),((0,0),Empty NotChecked),((0,2),Empty NotChecked),((1,1),Ship Checked),((2,0),Empty NotChecked),((2,2),Empty NotChecked)]) (aiPrio [((0,0),Empty NotChecked),((0,1),Empty Checked),((0,2),Empty NotChecked),((1,0),Ship Checked),((1,1),Ship Checked),((1,2),Ship NotChecked),((2,0),Empty NotChecked),((2,1),Empty Checked),((2,2),Empty NotChecked)] [])
test34 = TestCase $ assertEqual "filterShootList" ([((1,2),Ship NotChecked),((0,2),Empty NotChecked),((0,0),Empty NotChecked),((2,2),Empty NotChecked),((2,0),Empty NotChecked)],StdGen {unStdGen = SMGen 15835914885811367975 614480483733483467}) (filterShootList (array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty Checked),((0,2),Empty NotChecked),((1,0),Ship Checked),((1,1),Ship Checked),((1,2),Ship NotChecked),((2,0),Empty NotChecked),((2,1),Empty Checked),((2,2),Empty NotChecked)]) (mkStdGen 10))
test35 = TestCase $ assertEqual "removeChecked" ([((0,0),Empty NotChecked),((0,2),Empty NotChecked),((1,2),Ship NotChecked),((2,0),Empty NotChecked),((2,2),Empty NotChecked)]) (removeChecked [((0,0),Empty NotChecked),((0,1),Empty Checked),((0,2),Empty NotChecked),((1,0),Ship Checked),((1,1),Ship Checked),((1,2),Ship NotChecked),((2,0),Empty NotChecked),((2,1),Empty Checked),((2,2),Empty NotChecked)])
test36 = TestCase $ assertEqual "updateStack" ([((0,0),Empty NotChecked)]) (updateStack [] [((0,0),Empty NotChecked),((1,2),Ship NotChecked)])
test37 = TestCase $ assertEqual "cohesiveCells" ([((1,1),Ship Checked),((0,0),Empty NotChecked),((2,0),Empty NotChecked)]) (cohesiveCells (array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty Checked),((0,2),Empty NotChecked),((1,0),Ship Checked),((1,1),Ship Checked),((1,2),Ship NotChecked),((2,0),Empty NotChecked),((2,1),Empty Checked),((2,2),Empty NotChecked)]) ((1,0),Ship Checked))
test38 = TestCase $ assertEqual "isShip" True (isShip ((1,1),Ship NotChecked))
test39 = TestCase $ assertEqual "aiShootAux" (array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),((1,0),Empty Checked),((1,1),Empty NotChecked)],[]) (aiShootAux (array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),((1,0),Empty NotChecked),((1,1),Empty NotChecked)],[((1,0),Empty NotChecked)]) [((0,0),Ship NotChecked),((1,1),Empty NotChecked)])
test40 = TestCase $ assertEqual "aiShoot" ((array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),((1,0),Empty Checked),((1,1),Empty NotChecked)],[]),StdGen {unStdGen = SMGen 15835914885811367975 614480483733483467}) (aiShoot (array ((0,0),(1,1)) [((0,0),Ship NotChecked),((0,1),Ship Checked),((1,0),Empty NotChecked),((1,1),Empty NotChecked)],[((1,0),Empty NotChecked)]) (mkStdGen 10))
test41 = TestCase $ assertEqual "shipsCount" 0 (shipsCount initBoard)
test42 = TestCase $ assertEqual "cellCount" 0 (cellCount (Ship Checked) initBoard)

-- placing user --
test1 = TestCase $ assertEqual "placeShip: increasing correct ammount of ship-cells on board" (shipsCount initBoard + 5) (shipsCount b)
        where b = (gameBoardUser (placeShip (initGame {gameBoardAI = initBoard}) (0,0) 5 Horizontal))
test2 = TestCase $ assertEqual "placeShip edgeCase: shipsize 0" game (placeShip game (0,0) 0 Vertical)
        where game = (initGame {gameBoardAI = initBoard})
test3 = TestCase $ assertEqual "placeShip edgeCase: start position outside of board" initGame (placeShip initGame (-1,-1) 0 Vertical)
test4 = TestCase $ assertEqual "placeShip edgeCase: end position outside of board" initGame (placeShip initGame (9,9) 5 Horizontal)


-- placing AI --
test5 = TestCase $ assertEqual "placeMultipleShipsAI: correct total ship-cells" 17 (shipsCount b)
        where b = fst $ placeMultipleShipsAI (mkStdGen 10) initBoard initShips

--shooting user --
test6 = TestCase $ assertEqual "playerShoot: shooting empty cell " (Empty Checked) (getCell (gameBoardAI (playerShoot initGame (0,0))) (0,0))

-- shooting AI --

test7 = TestCase $ assertEqual "aiShoot: shooting empty" 1  (cellCount (Empty Checked) b)
        where ((b, _),_)= aiShoot (initBoard, []) (mkStdGen 10)

test8 = TestCase $ assertEqual "aiShoot: shooting ship" 1 (cellCount (Ship Checked) b)
        where ((b, _),_)= aiShoot (shipBoard, []) (mkStdGen 10)
              shipBoard = array ((0, 0), (9,9)) $ zip (range ((0, 0), (9,9))) (repeat $ Ship NotChecked)


-- run --
tests = TestList    [ test1
                    , test2
                    {-, test3
                    , test4
                    , test5
                    , test6
                    , test7
                    , test8
                    , test9
                    , test10
                    , test11
                    , test12-}
                    ]

runtests = runTestTT $ tests
