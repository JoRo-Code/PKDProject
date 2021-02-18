module Graphics where

import Data.Array

import Graphics.Gloss

import Game

n :: Int
n = 3

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n

waterOfBoard = undefined
shipsOfBoard = undefined
missesOfBoard = undefined
hitsOfBoard  = undefined

boardGrid :: Picture
boardGrid =
    pictures
    $ concatMap (\i -> [ line [ (i * cellWidth, 0.0)
                              , (i * cellWidth, fromIntegral screenHeight)
                              ]
                       , line [ (0.0,                      i * cellHeight)
                              , (fromIntegral screenWidth, i * cellHeight)
                              ]
                       ])
      [0.0 .. fromIntegral n]

boardAsPicture :: Board -> Picture
boardAsPicture board = 
    pictures [ waterOfBoard board
            , shipsOfBoard  board
            , missesOfBoard board
            , hitsOfBoard   board
            , boardGrid
            ]

main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)


--  ghc -threaded graphics
-- https://mmhaskell.com/blog/2019/3/25/making-a-glossy-game-part-1
-- https://hackage.haskell.org/package/gloss-examples-1.13.0.3/src/
