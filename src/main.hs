module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import System.Random

import Game         -- initialGame
import Logic        -- eventHandler
import Rendering    -- drawGame
import Animation    -- animationFunc


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

-- window specifications
window :: Display
window = InWindow "BattleShips" (floor screenWidth, floor screenHeight) (100, 100)

backgroundColor :: Color
backgroundColor = makeColor 0 0 0 0

fps :: Int
fps = 30




{- main
       main loop which controls output in gloss
-}
main :: IO ()
main = do
       gen <- getStdGen
       let (initGameBoardAI, newGen) =  placeMultipleShipsAI gen (gameBoardAI initGame) initShips
       let aiBoards = listOfBoards 1000 gen initBoard initShips
       play window backgroundColor fps initGame {gameBoardAI = head aiBoards, gameBoardsAI = tail aiBoards, gen = gen} drawGame eventHandler animationFunc


