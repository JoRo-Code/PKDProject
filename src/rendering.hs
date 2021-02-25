module Rendering where

import Graphics.Gloss
import Data.Array
import Game

boardGridColor = makeColorI 255 255 255 255

type BoardPos = (ScreenCoord, ScreenCoord)

data GameColor = CellColor {miss :: Color, 
                            hit  :: Color, 
                            placedShip :: Color,
                            movingShip :: Color}
                 | BoardColor {grid :: Color}

cellColor :: GameColor
cellColor = CellColor {miss = white, hit = red, placedShip = greyN 0.5, movingShip = greyN 0.7}


screenWidth :: Float
screenWidth = 1500
screenHeight :: Float
screenHeight = (screenWidth - screenDivider) / 2

-- filling between both boards
screenDivider :: Float
screenDivider = 300

--------------------- Solving cellWidth and cellHeight problem ---------------------


cellWidth :: Float
cellWidth  = (screenWidth - screenDivider) * 0.5 / fromIntegral n
cellHeight :: Float
cellHeight = screenHeight / fromIntegral n
boardWidth :: Float
boardWidth = (screenWidth - screenDivider) / 2
boardHeight :: Float
boardHeight = screenHeight

--------------------- Solving cellWidth and cellHeight problem ---------------------

boardAIPos :: BoardPos
boardAIPos = ((screenWidth * 0.5 + screenDivider * 0.5 ,0), (screenWidth, screenHeight))

boardUserPos :: BoardPos
boardUserPos = ((0,0), (screenWidth * 0.5 - screenDivider * 0.5, screenHeight))

{- puts a picture to a specific cell's screenCoordinates -} 
snapPictureToCell :: Picture -> BoardPos -> CellCoord -> Picture
snapPictureToCell picture boardPos@((x1,y1),(x2,y2)) (c, r) = translate x y picture
    where x = x1 + fromIntegral c * cellWidth + cellWidth / 2
          y = y1 + fromIntegral r * cellHeight + cellHeight / 2
          

crossPicture :: Picture
crossPicture  = pictures [ rotate 45 $ rectangleSolid length thickness
                 , rotate (-45) $ rectangleSolid length thickness
                 ]
                 where length = 0.7 * min cellWidth cellHeight
                       thickness = 0.15 * min cellWidth cellHeight

shipPicture :: Picture
shipPicture = pictures [ rectangleSolid (0.7 * cellWidth) (0.7 * cellHeight)]
                  
placingShipPicture :: CellCoord -> Direction -> ShipSize -> Picture
placingShipPicture (c, r) Horizontal s =  translate (cellWidth * (0.5 * fromIntegral s + fromIntegral c)) (cellHeight * (0.5 + fromIntegral r)) 
                                          (pictures [ rectangleSolid (cellWidth * fromIntegral s) cellHeight])
placingShipPicture (c, r) Vertical s   =  translate (cellWidth * (fromIntegral c + 0.5)) (cellHeight * (1 + fromIntegral r - 0.5 * fromIntegral s)) 
                                          (pictures [ rotate 90 $ rectangleSolid (cellWidth * fromIntegral s) cellHeight])                        

cellsToPicture :: Board -> BoardPos -> Cell -> Picture -> Picture
cellsToPicture board pos c pic =  pictures
                            $ map (snapPictureToCell pic pos . fst)
                            $ filter (\(_, e) -> e == c)
                            $ assocs board

boardGrid :: BoardPos -> Picture
boardGrid boardPos@((x1,y1),(x2,y2)) =
    pictures
    $ concatMap (\i -> [ line [ (x1 + i * cellWidth, 0.0)
                              , (x1 + i * cellWidth,  boardHeight)
                              ]
                       , line [ (x1 + 0.0, i * cellHeight)
                              , (x1 + boardWidth, i * cellHeight)
                              ]
                       ])
      [0.0 .. fromIntegral n]


boardAIGrid :: Picture
boardAIGrid = boardGrid boardAIPos

boardUserGrid :: Picture
boardUserGrid = boardGrid boardUserPos

displayGameStage :: GameStage -> BoardPos -> Picture
displayGameStage stage ((x1,y1),(x2,y2)) = translate (x2 + 0.15 * screenDivider) (y2 - 0.4 * screenDivider) $ pictures [scale sc sc $ text s]
                                                    where sc = screenDivider / 1250
                                                          s = case stage of
                                                              Placing User  -> "Place ships!"
                                                              Shooting User -> "Shoot enemy!"
 
displayWinner :: Maybe Player -> BoardPos -> Picture
displayWinner Nothing _ = Blank
displayWinner player ((x1,y1),(x2,y2)) = translate (x2 + 0.15 * screenDivider) (y2 - 0.4* screenDivider) $ pictures [scale sc sc $ text s]
                                         where sc = screenDivider / 1250
                                               s = case player of
                                                   Just User -> "You won!"
                                                   Just AI   -> "You lost!"

displayCurrentRound :: Int -> BoardPos -> Picture
displayCurrentRound round boardPos@((x1,y1),(x2,y2)) = translate (x2 + 0.15 * screenDivider) (y2 - 0.55 * screenDivider) $ pictures [scale sc sc $ text $ "Round " ++ show round]
                                                        where sc = screenDivider / 1250

missToPicture :: Board -> BoardPos -> Picture
missToPicture board pos = cellsToPicture board pos (Empty Checked) crossPicture

hitsToPicture :: Board -> BoardPos -> Picture
hitsToPicture board pos = cellsToPicture board pos (Ship Checked) shipPicture

shipsToPicture :: Board -> BoardPos -> Picture
shipsToPicture board pos = cellsToPicture board pos (Ship NotChecked) shipPicture

showPlacingShip :: Ships -> Picture
showPlacingShip [] = Blank
showPlacingShip ((coord, d, s): xs) = placingShipPicture coord d s 

gameAsRunningPicture :: Game -> Picture
gameAsRunningPicture game =
    pictures [color boardGridColor boardAIGrid,
              color boardGridColor boardUserGrid,
              color boardGridColor $
              color (miss cellColor) $ missToPicture userBoard boardUserPos,
              if win == Nothing then color white $ displayGameStage stage boardUserPos else Blank,
              color white $ displayCurrentRound currRound boardUserPos,
              color white $ displayWinner win boardUserPos,
              color (miss cellColor) $ missToPicture boardAI boardAIPos,
              color (placedShip cellColor) $ shipsToPicture userBoard boardUserPos,
              color (hit cellColor)  $ hitsToPicture userBoard boardUserPos,
              color (hit cellColor)  $ hitsToPicture boardAI boardAIPos,
              color (movingShip cellColor) $ showPlacingShip ships
             ]
             where userBoard = gameBoardUser game
                   boardAI = gameBoardAI game
                   ships = shipsUser game
                   stage = gameStage game
                   win = winner game
                   currRound = currentRound game

drawGame :: Game -> Picture
drawGame game = translate (screenWidth * (-0.5))
                          (screenHeight * (-0.5))
                           frame
        where frame = gameAsRunningPicture game