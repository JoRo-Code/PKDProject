module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Game         -- initialGame
import Logic        -- eventHandler
import Rendering    -- drawGame

import System.Random

placeMultipleShipsAI :: Board -> Ships -> Board
placeMultipleShipsAI b [] = b
placeMultipleShipsAI b (ship:ships) = placeMultipleShipsAI (placeShipAI b s (findAllValidPlacements b s)) ships
                                     where (_, _, s) = ship

randomPlacement :: [(CellCoord, Direction)] -> (CellCoord, Direction)
randomPlacement = undefined

placeShipAI :: Board -> ShipSize -> [(CellCoord, Direction)] -> Board
placeShipAI b s placements = placeShipAux b coord s d
                           where (coord , d) = head placements -- call random function here
                           


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
main = play
       window
       backgroundColor
       fps
       initGame
       drawGame
       eventHandler --eventHandler
       (const id)
