module Rendering where

import Graphics.Gloss

import Game

boardGridColor = makeColorI 255 255 255 255


screenWidth :: Int
screenWidth = 640
screenHeight :: Int
screenHeight = 460

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n


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


boardAsRunningPicture :: Board -> Picture
boardAsRunningPicture board =
    pictures [ --color playerXColor $ xCellsOfBoard board
             --, color playerOColor $ oCellsOfBoard board
              color boardGridColor $ boardGrid
             ]
             
drawGame :: Game -> Picture
drawGame game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
        where frame = boardAsRunningPicture (gameBoardUser game)