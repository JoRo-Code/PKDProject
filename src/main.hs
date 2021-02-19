module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Game         -- initialGame
import Logic        -- eventHandler
import Rendering    -- drawGame

screenWidth = 640
screenHeight = 460
window = InWindow "BattleShips" (screenWidth, screenHeight) (100, 100)

backgroundColor = makeColor 0 0 100 255
fps = 30

main :: IO ()
main = play
       window
       backgroundColor
       fps
       initialGame
       drawGame
       eventHandler
       (const id)


