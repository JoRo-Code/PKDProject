module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Game         -- initialGame
import Logic        -- eventHandler
import Rendering    -- drawGame


window = InWindow "BattleShips" (screenWidth, screenHeight) (100, 100)

backgroundColor = makeColor 0 0 0 0
fps = 30

main :: IO ()
main = play
       window
       backgroundColor
       fps
       initialGame
       drawGame
       eventHandler --eventHandler
       (const id)


