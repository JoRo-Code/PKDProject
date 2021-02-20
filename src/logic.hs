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

getCell :: Board -> CellCoord -> Cell
getCell b c = b ! c

-- PRE: coordinates are in range, cell is NotChecked
-- Changes the state of a cell to checked
checkCell :: Board -> CellCoord -> Board
checkCell b (c, r) = case getCell b (c, r) of
                      Empty NotChecked -> b // [((c, r), Empty Checked)]   
                      Ship NotChecked  -> b // [((c, r), Ship Checked)]
                      _ -> b

playerTurn :: Game -> CellCoord -> Game
playerTurn game coord = game {gameBoardAI = checkCell (gameBoardAI game) coord}

mousePosToCoordinates :: ScreenCoord -> CellCoord
mousePosToCoordinates (x, y) = (floor x, floor y)

mouseToCell :: ScreenCoord -> BoardPos -> CellCoord
mouseToCell (x, y) boardPos@((x1,y1),(x2,y2)) = (floor ((y - y1) / cellHeight (boardHeight boardPos)),
                                                 floor ((x - x1) / cellWidth (boardWidth boardPos)))

mouseToBoard :: ScreenCoord -> BoardPos
mouseToBoard (x, y) = undefined


eventHandler :: Event -> Game -> Game
eventHandler (EventKey (MouseButton LeftButton) Up _ mousePos) game = 
    --playerTurn game $ mousePosAsCellCoord mousePos
    playerTurn game $ mouseToCell mousePos placingBoardPos 
eventHandler _ game = game 
