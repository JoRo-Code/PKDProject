module Rendering where

import Graphics.Gloss

import Game

boardGridColor = makeColorI 255 255 255 255


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



boardAsRunningPicture board =
    pictures [ --color playerXColor $ xCellsOfBoard board
             --, color playerOColor $ oCellsOfBoard board
              color boardGridColor $ boardGrid
             ]
drawGame game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
        where frame = boardAsRunningPicture (gameBoard game)