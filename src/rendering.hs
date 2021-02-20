module Rendering where

import Graphics.Gloss

import Game

boardGridColor = makeColorI 255 255 255 255

type BoardPos = ((Float,Float),(Float,Float))

screenWidth :: Int
screenWidth = 1400
screenHeight :: Int
screenHeight = 700

cellWidth width n =  width / fromIntegral n

cellHeight height n =  height / fromIntegral n

shootingBoardPos :: BoardPos
shootingBoardPos = ((fromIntegral screenWidth*0.5,0),(fromIntegral screenWidth, fromIntegral screenHeight))

placingBoardPos :: BoardPos
placingBoardPos = ((0,0),(fromIntegral screenWidth*0.5, fromIntegral screenHeight))

{-
    ( ((fromIntegral screenWidth)*0.5,                             0),
                     (fromIntegral screenWidth,         fromIntegral screenHeight) )
-}

boardWidth ((x1,y1),(x2,y2)) = x2-x1
boardHeight ((x1,y1),(x2,y2)) = y2-y1

--boardGrid :: Picture
boardGrid n boardPos@((x1,y1),(x2,y2)) =
    pictures
    $ concatMap (\i -> [ line [ (x1 + i * cellW, 0.0)
                              , (x1 + i * cellW,  -boardH)
                              ]
                       , line [ (x1 + 0.0, i * (-cellH))
                              , (x1 + boardW, i * (-cellH))
                              ]
                       ])
      [0.0 .. fromIntegral n]
      where cellW = cellWidth (boardWidth boardPos) n
            cellH = cellHeight (boardHeight boardPos) n
            boardH = boardHeight boardPos
            boardW = boardWidth boardPos

foo :: Picture
foo  = pictures [ rotate 45.0 $ rectangleSolid 50 10.0
                 , rotate (-45.0) $ rectangleSolid 50 10.0
                 ]



shootingBoardGrid = boardGrid 10 shootingBoardPos
placingBoardGrid = boardGrid 10 placingBoardPos


boardAsRunningPicture :: Board -> Picture
boardAsRunningPicture board =
    pictures [ --color playerXColor $ xCellsOfBoard board
             --, color playerOColor $ oCellsOfBoard board
              color boardGridColor shootingBoardGrid,
              color boardGridColor placingBoardGrid,
              color boardGridColor $ translateCorrect (100, 100) foo 
             ]

translateCorrect :: (Float, Float) -> Picture -> Picture
translateCorrect (width, height) = translate width $ negate height


drawGame :: Game -> Picture
drawGame game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * 0.5)
                               frame
        where frame = boardAsRunningPicture (gameBoardUser game)