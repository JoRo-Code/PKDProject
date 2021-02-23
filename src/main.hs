module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Game         -- initialGame
import Logic        -- eventHandler
import Rendering    -- drawGame
import System.Random


placeMultipleShipsAI :: StdGen -> Board -> Ships -> Board
placeMultipleShipsAI gen b [] = b
placeMultipleShipsAI gen b ((_, _, s):ships) = placeMultipleShipsAI newGen newBoard ships
                                      where (newBoard, newGen) = placeShipAI gen b s (findAllValidPlacements b s)

randomPlacement :: StdGen ->  [(CellCoord, Direction)]  -> ((CellCoord, Direction), StdGen)
randomPlacement gen list = (list !! randomInt, newGen)
                     where range = (0, length list - 1)
                           (randomInt, newGen) = randomR range gen

placeShipAI :: StdGen -> Board -> ShipSize -> [(CellCoord, Direction)] -> (Board, StdGen)
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
       let initGameBoardAI =  placeMultipleShipsAI gen (gameBoardAI initGame) initShips
       play window backgroundColor fps initGame {gameBoardAI = initGameBoardAI} drawGame eventHandler  (const id)


