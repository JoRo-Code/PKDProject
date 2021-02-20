{-# LANGUAGE LambdaCase #-} 
module Rendering where

import Graphics.Gloss
import Data.Array
import Game



boardGridColor = makeColorI 255 255 255 255

type BoardPos = (ScreenCoord, ScreenCoord)

screenWidth :: Int
screenWidth = 1400
screenHeight :: Int
screenHeight = 700


cellWidth width =  width / fromIntegral n

cellHeight height =  height / fromIntegral n

shootingBoardPos :: BoardPos
shootingBoardPos = ((fromIntegral screenWidth*0.5,0),(fromIntegral screenWidth, fromIntegral screenHeight))

placingBoardPos :: BoardPos
placingBoardPos = ((0,0),(fromIntegral screenWidth*0.5, fromIntegral screenHeight))


boardWidth ((x1,y1),(x2,y2)) = x2-x1
boardHeight ((x1,y1),(x2,y2)) = y2-y1

snapPictureToCell picture boardPos@((x1,y1),(x2,y2)) (row, column) = translate x y picture
    where x = x1 + fromIntegral column * cellW + cellW / 2
          y = y1 + fromIntegral row * cellH + cellH / 2
          cellW = cellWidth (boardWidth boardPos)
          cellH = cellHeight (boardHeight boardPos)
 


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

foo :: Picture
foo  = pictures [ rotate 45.0 $ rectangleSolid 50 10.0
                 , rotate (-45.0) $ rectangleSolid 50 10.0
                 ]

cellsToPicture :: Board -> BoardPos -> Cell -> Picture -> Picture
cellsToPicture board pos c pic =  pictures
                            $ map (snapPictureToCell pic pos . fst)
                            $ filter (\(_, e) -> e == c)
                            $ assocs board

checked board pos = cellsToPicture board pos (Empty Checked) foo

shootingBoardGrid = boardGrid shootingBoardPos
placingBoardGrid = boardGrid placingBoardPos


boardAsRunningPicture :: Board -> Picture
boardAsRunningPicture board =
    pictures [ --color playerXColor $ xCellsOfBoard board
             --, color playerOColor $ oCellsOfBoard board
              color boardGridColor shootingBoardGrid,
              color boardGridColor placingBoardGrid,
              color boardGridColor $
              color boardGridColor $ snapPictureToCell foo shootingBoardPos (9, 2),
              color green $ snapPictureToCell foo placingBoardPos (0, 0),
              color green $ checked board shootingBoardPos
             ]

drawGame :: Game -> Picture
drawGame game = translate (fromIntegral screenWidth * (-0.5))
                          (fromIntegral screenHeight * (-0.5))
                           frame
        where frame = boardAsRunningPicture (gameBoardUser game)