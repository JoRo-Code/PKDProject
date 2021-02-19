module Logic where

import Graphics.Gloss.Interface.Pure.Game
import Data.Array

import Game





{- 
computes the cellcoordinates for a specific (x,y) on the screen
-}
mousePosAsCellCoord :: (Float, Float) -> CellCoordinates


{- transforms the game if a player clicks on a cell -}
playerTurn :: Game -> CellCoordinates -> Game


{- 
handles the inputs of the game.
specifically what happends if the mouse is pressed
-}
eventHandler (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    playerTurn game $ mousePosAsCellCoord mousePos
eventHandler _ game = game 