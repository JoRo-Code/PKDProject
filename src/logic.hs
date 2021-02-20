module Logic where

import Graphics.Gloss.Interface.Pure.Game
import Data.Array

import Game
import Rendering






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
                      _ -> b

playerTurn :: Game -> Coordinates -> Game
playerTurn game coord = game {gameBoardUser = checkCell (gameBoardUser game) coord}

mousePosToCoordinates :: (Float, Float) -> Coordinates
mousePosToCoordinates (x, y) = (floor x, floor y)

mouseToCell :: (Float, Float) -> BoardPos -> Coordinates
mouseToCell (x, y) boardPos@((x1,y1),(x2,y2)) = (floor ((x - x1) / cellWidth (boardWidth boardPos)),
                                                 floor ((y - y1) / cellHeight (boardHeight boardPos)))

mouseToBoard :: (Float, Float) -> BoardPos
mouseToBoard (x, y) = undefined


eventHandler :: Event -> Game -> Game
eventHandler (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    --playerTurn game $ mousePosAsCellCoord mousePos
    playerTurn game $ mouseToCell mousePos placingBoardPos

eventHandler _ game = game 
