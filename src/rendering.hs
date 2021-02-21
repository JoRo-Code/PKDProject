{-# LANGUAGE LambdaCase #-} 
module Rendering where

import Graphics.Gloss
import Data.Array
import Game



boardGridColor = makeColorI 255 255 255 255

type BoardPos = (ScreenCoord, ScreenCoord)

shipColor :: Color
shipColor = greyN 0.5
hitColor :: Color
hitColor = red
missColor :: Color
missColor = white


screenWidth :: Int
screenWidth = 1500
screenHeight :: Int
screenHeight = 700

-- filling between both boards
screenDivider :: Float
screenDivider = 100

cellWidth width =  width / fromIntegral n

cellHeight height =  height / fromIntegral n

boardAIPos :: BoardPos
boardAIPos = ((fromIntegral screenWidth*0.5 + screenDivider*0.5 ,0),(fromIntegral screenWidth, fromIntegral screenHeight))

boardUserPos :: BoardPos
boardUserPos = ((0,0),(fromIntegral screenWidth*0.5-screenDivider*0.5, fromIntegral screenHeight))


boardWidth ((x1,y1),(x2,y2)) = x2-x1
boardHeight ((x1,y1),(x2,y2)) = y2-y1

{- puts a picture to a specific cell's screenCoordinates -} 
snapPictureToCell picture boardPos@((x1,y1),(x2,y2)) (column, row) = translate x y picture
    where x = x1 + fromIntegral row * cellW + cellW / 2
          y = y1 + fromIntegral column * cellH + cellH / 2
          cellW = cellWidth (boardWidth boardPos)
          cellH = cellHeight (boardHeight boardPos)
 


crossPicture :: Picture
crossPicture  = pictures [ rotate 45.0 $ rectangleSolid (0.7 * cellW) (0.15 * cellW)
                 , rotate (-45.0) $ rectangleSolid (0.7 * cellW) (0.15 * cellW)
                 ]
                 where cellW = cellWidth (boardWidth boardUserPos)

shipPicture :: Picture
shipPicture = pictures [ rectangleSolid cellW cellH]
                        where cellW = cellWidth (boardWidth boardUserPos)
                              cellH = cellHeight (boardHeight boardUserPos)
 

cellsToPicture :: Board -> BoardPos -> Cell -> Picture -> Picture
cellsToPicture board pos c pic =  pictures
                            $ map (snapPictureToCell pic pos . fst)
                            $ filter (\(_, e) -> e == c)
                            $ assocs board

--boardGrid :: Picture
boardGrid boardPos@((x1,y1),(x2,y2)) =
    pictures
    $ concatMap (\i -> [ line [ (x1 + i * cellW, 0.0)
                              , (x1 + i * cellW,  boardH)
                              ]
                       , line [ (x1 + 0.0, i * cellH)
                              , (x1 + boardW, i * cellH)
                              ]
                       ])
      [0.0 .. fromIntegral n]
      where cellW = cellWidth (boardWidth boardPos)
            cellH = cellHeight (boardHeight boardPos)
            boardH = boardHeight boardPos
            boardW = boardWidth boardPos


boardAIGrid = boardGrid boardAIPos
boardUserGrid = boardGrid boardUserPos


miss board pos = cellsToPicture board pos (Empty Checked) crossPicture
hit board pos = cellsToPicture board pos (Ship Checked) crossPicture

ship board pos = cellsToPicture board pos (Ship NotChecked) shipPicture

boardAsRunningPicture :: Board -> Board -> Picture
boardAsRunningPicture userBoard boardAI =
    pictures [color boardGridColor boardAIGrid,
              color boardGridColor boardUserGrid,
              color boardGridColor $
              --color boardGridColor $ snapPictureToCell crossPicture boardAIPos (9, 2),
              color missColor $ miss userBoard boardUserPos,
              color missColor $ miss boardAI boardAIPos,
              color shipColor $ ship userBoard boardUserPos,
              color hitColor  $ hit userBoard boardUserPos,
              color hitColor  $ hit boardAI boardAIPos
             ]

drawGame :: Game -> Picture
drawGame game = translate (fromIntegral screenWidth * (-0.5))
                          (fromIntegral screenHeight * (-0.5))
                           frame
        where frame = boardAsRunningPicture (gameBoardUser game) (gameBoardAI game)