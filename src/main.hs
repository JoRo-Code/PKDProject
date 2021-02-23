module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Game         -- initialGame
import Logic        -- eventHandler
import Rendering    -- drawGame
import System.Random

--placeMultipleShipsAI :: (RandomGen g) => g -> Board -> Ships -> Board
placeMultipleShipsAI gen b [] = b
placeMultipleShipsAI gen b (ship:ships) = placeMultipleShipsAI newGen (newBoard) ships
                                      where (_, _, s) = ship
                                            (newBoard, newGen) = placeShipAI gen b s (findAllValidPlacements b s)


--randomPlacement :: (RandomGen g) => g -> [(CellCoord, Direction)] -> ((CellCoord, Direction), g)
randomPlacement gen list = (list !! randomInt, newGen)
                     where range = (0, length list - 1)
                           (randomInt, newGen) = randomR range gen ::  (Int, StdGen)

--placeShipAI :: (RandomGen g) => g -> Board -> ShipSize -> [(CellCoord, Direction)] -> (Board, g)
placeShipAI gen b s placements = (placeShipAux b coord s d, newGen)
                             where ((coord , d), newGen) = randomPlacement gen placements --randomPlacement placements -- call random function here
                           
findAllValidPlacements :: Board -> ShipSize -> [(CellCoord, Direction)]
findAllValidPlacements b s = findValidDirectionalPlacements b allCoords s Horizontal ++ findValidDirectionalPlacements b allCoords s Vertical 

findValidDirectionalPlacements :: Board -> [CellCoord] -> ShipSize ->  Direction -> [(CellCoord, Direction)]
findValidDirectionalPlacements b coords s d = map (\coord -> (coord, d)) $ filter (\coord -> validShipPlacement b coord s d) coords
                 
allCoords :: [CellCoord]
allCoords = [(c, r) | c <- [0..n-1], r <- [0..n-1]]




window :: Display
window = InWindow "BattleShips" (floor screenWidth, floor screenHeight) (100, 100)

backgroundColor :: Color
backgroundColor = makeColor 0 0 0 0

fps :: Int
fps = 30

main :: IO ()
main = do
       gen <- getStdGen
       let
       play
       window
       backgroundColor
       fps
       initGame {gameBoardAI = placeMultipleShipsAI gen (gameBoardAI initGame) initShips}
       drawGame
       eventHandler --eventHandler
       (const id)


