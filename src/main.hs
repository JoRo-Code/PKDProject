module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Game         -- initialGame
import Logic        -- eventHandler
import Rendering    -- drawGame

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


