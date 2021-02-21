module Logic where

import Graphics.Gloss.Interface.Pure.Game
import Data.Array

import Game
import Rendering

import Debug.Trace

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

validCoordinates :: CellCoord -> Bool
validCoordinates  = inRange boardIndex
                    where boardIndex = ((0, 0), (n - 1, n - 1)) 

getCell :: Board -> CellCoord -> Cell
getCell b c = b ! c



-- PRE: coordinates are in range, cell is NotChecked
-- Changes the state of a cell to checked
checkCell :: Board -> CellCoord -> Board
checkCell b (c, r) = case getCell b (c, r) of
                      Empty NotChecked -> b // [((c, r), Empty Checked)]   
                      Ship NotChecked  -> b // [((c, r), Ship Checked)]
                      _ -> b

-- PRE: coordinates are in range
-- returns the state of the cell at coordinates
getState :: Board -> CellCoord -> SquareState
getState b c =  case getCell b c of
                     Empty s -> s
                     Ship s -> s

-- returns True if a cell is Checked, else False
isChecked :: Board -> CellCoord -> Bool
isChecked b coord = s == Checked
        where s = getState b coord

playerShoot :: Game -> CellCoord -> Game
playerShoot game coord | validCoordinates coord -- && not isChecked
                        = game {gameBoardUser = checkCell (gameBoardUser game) coord, gameStage = 
                            if gameStage game == Shooting User then Shooting AI else Shooting User}
                       | otherwise = game

playerPlace = undefined

mousePosToCoordinates :: ScreenCoord -> CellCoord
mousePosToCoordinates (x, y) = (floor x, floor y)

mouseToCell :: ScreenCoord -> BoardPos -> CellCoord
mouseToCell (x, y) boardPos@((x1,y1),(x2,y2)) = let (kek1, kek2) = (div n 2 + floor ((y - y1) / cellHeight (boardHeight boardPos)),
                                                                    n + floor ((x - x1) / cellWidth (boardWidth boardPos)))
                                                                    in trace (show ((kek1, kek2), (x,y))) $ (kek1, kek2)

mouseToBoard :: ScreenCoord -> BoardPos
mouseToBoard (x, y) = undefined

eventHandler :: Event -> Game -> Game
eventHandler (EventKey (MouseButton LeftButton) Up _ mousePos) game = 
    case gameStage game of
        Shooting User -> playerShoot game $ mouseToCell mousePos shootingBoardPos -- should change gamestage to shooting AI
        Placing User -> playerShoot game $ mouseToCell mousePos placingBoardPos -- change to a function that places ships instead
        Shooting AI -> playerShoot game $ mouseToCell mousePos shootingBoardPos -- calls AI shoot function and goes back to shooting user state, unless AI wins
        Placing AI -> undefined -- calls AI place function, and goes to shooting user state and start the game
    --playerTurn game $ mousePosAsCellCoord mousePos
    -- changing to placingBoardPos will let us place in the first grid
eventHandler _ game = game 
