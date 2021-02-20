module Logic where

import Graphics.Gloss.Interface.Pure.Game
import Data.Array

import Game






{- 
computes the cellcoordinates for a specific (x,y) on the screen
-}
--mousePosAsCellCoord :: (Float, Float) -> CellCoordinates


{- transforms the game if a player clicks on a cell -}
--playerTurn :: Game -> CellCoordinates -> Game


{- 
handles the inputs of the game.
specifically what happends if the mouse is pressed
-}

getCell :: Board -> Coordinates -> Cell
getCell b c = b ! c

-- PRE: coordinates are in range, cell is NotChecked
-- Changes the state of a cell to checked
checkCell :: Board -> Coordinates -> Board
checkCell b (r, c) = case getCell b (r, c) of
                      Empty NotChecked -> b // [((r,c), Empty Checked)]   
                      Ship NotChecked  -> b // [((r,c), Ship Checked)]

playerTurn :: Game -> Coordinates -> Game
playerTurn game coord = game {gameBoardUser = checkCell (gameBoardUser game) (0,0)}

mousePosToCoordinates :: (Float, Float) -> Coordinates
mousePosToCoordinates (x, y) = (floor x, floor y)

eventHandler (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    --playerTurn game $ mousePosAsCellCoord mousePos
    playerTurn game $ mousePosToCoordinates mousePos

eventHandler _ game = game 
