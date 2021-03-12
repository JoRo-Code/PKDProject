module Main where
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Game         -- initialGame
import Logic        -- eventHandler
import Rendering    -- drawGame
import Animation    -- animationFunc

-- window specifications
window :: Display
window = InWindow "BattleShips" (floor screenWidth, floor screenHeight) (100, 100)

backgroundColor :: Color
backgroundColor = makeColor 0 0 0 0

fps :: Int
fps = 30

{- main
       main loop which controls output in gloss
       SIDEEFFECTS: gloss-window
       
-}
main :: IO ()
main = do
       gen <- getStdGen
       let (initGameBoardAI, newGen) =  placeMultipleShipsAI gen initBoard initShips
       play 
              window 
              backgroundColor 
              fps 
              initGame      {gameBoardAI = initGameBoardAI, gen = newGen} 
              drawGame 
              eventHandler 
              animationFunc
