module Rendering where

import Graphics.Gloss

import Game

boardGridColor = makeColorI 255 255 255 255

type BoardPos = ((Float,Float),(Float,Float))

screenWidth :: Int
screenWidth = 1400
screenHeight :: Int
screenHeight = 860

cellWidth width n =  width / fromIntegral n

cellHeight height n =  height / fromIntegral n

shootingBoardPos :: BoardPos
shootingBoardPos = (((fromIntegral screenWidth)*0.5,0),(fromIntegral screenWidth, fromIntegral screenHeight))

{-
    ( ((fromIntegral screenWidth)*0.5,                             0),
                     (fromIntegral screenWidth,         fromIntegral screenHeight) )
-}

boardWidth ((x1,y1),(x2,y2)) = x2-x1
boardHeight ((x1,y1),(x2,y2)) = y2-y1



shootingBoardWidth :: Float
shootingBoardWidth = boardWidth shootingBoardPos

shootingBoardHeight :: Float
shootingBoardHeight = boardHeight shootingBoardPos


shootingBoardCellWidth = cellWidth shootingBoardWidth 10 
shootingBoardCellHeight = cellHeight shootingBoardHeight 10 


--boardGrid :: Picture
boardGrid cellWidth cellHeight boardPos@((x1,y1),(x2,y2)) =
    pictures
    $ concatMap (\i -> [ line [ (x1 + i * cellWidth, 0.0)
                              , (x1 + i * cellWidth,  boardHeight boardPos)
                              ]
                       , line [ (x1 + 0.0,                      i * cellHeight)
                              , (x1 + boardWidth boardPos, i * cellHeight)
                              ]
                       ])
      [0.0 .. fromIntegral n]


shootingBoardGrid = boardGrid shootingBoardCellWidth shootingBoardCellHeight shootingBoardPos

boardAsRunningPicture :: Board -> Picture
boardAsRunningPicture board =
    pictures [ --color playerXColor $ xCellsOfBoard board
             --, color playerOColor $ oCellsOfBoard board
              color boardGridColor $ shootingBoardGrid 
             ]
             
drawGame :: Game -> Picture
drawGame game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
        where frame = boardAsRunningPicture (gameBoardUser game)