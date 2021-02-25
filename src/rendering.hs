module Rendering where

import Graphics.Gloss
import Data.Array
import Game

boardGridColor = makeColorI 255 255 255 255

type BoardPos = (ScreenCoord, ScreenCoord)

textColor :: Color
textColor = white
missColor :: Color
missColor = white
hitColor :: Color
hitColor = red
shipColor :: Color
shipColor = greyN 0.5
movShipColor :: Color
movShipColor = greyN 0.7
radarColor :: Color
radarColor = makeColorI 0 100 0 255

radarThickness = 3



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
                  
movingShipPicture :: CellCoord -> Direction -> ShipSize -> Picture
movingShipPicture (c, r) Horizontal s =  translate (cellWidth * (0.5 * fromIntegral s + fromIntegral c)) (cellHeight * (0.5 + fromIntegral r)) 
                                          (pictures [ rectangleSolid (cellWidth * fromIntegral s) cellHeight])
movingShipPicture (c, r) Vertical s   =  translate (cellWidth * (fromIntegral c + 0.5)) (cellHeight * (1 + fromIntegral r - 0.5 * fromIntegral s)) 
                                          (pictures [ rotate 90 $ rectangleSolid (cellWidth * fromIntegral s) cellHeight])                        

explosionPicture :: Radius -> Picture
explosionPicture r = thickCircle r 10

moveExplosion :: Radius -> Pos -> Bool -> Picture 
moveExplosion _ _ False = Blank
moveExplosion r (x,y) _ = translate (screenWidth/2) (screenHeight/2) (translate x y (explosionPicture r))

radarPicture :: Radar -> Picture
radarPicture (r1, r2, r3, r4, r5) =     
    translate (screenWidth/2) (screenHeight/2) 
    (pictures 
    [
     thickCircle r1 radarThickness, 
     thickCircle r2 radarThickness,
     thickCircle r3 radarThickness, 
     thickCircle r4 radarThickness,
     thickCircle r5 radarThickness
    ]) 


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


displayGameName :: BoardPos -> Picture
displayGameName ((x1,y1),(x2,y2)) = translate (x2 + 0.05 * screenDivider) (y2 - 0.2 * screenDivider) $ pictures [scale sc sc $ text s]
                                                    where sc = screenDivider / 900
                                                          s = "BATTLESHIPS"

displayPlaceHere :: BoardPos -> Picture
displayPlaceHere ((x1,y1),(x2,y2)) = translate (x2 + 0.05 * screenDivider) (y1 + 0.2 * screenDivider) $ pictures [scale sc sc $ text s]
                                                    where sc = screenDivider / 1400
                                                          s = "<---- Place here"

displayShootHere :: BoardPos -> Picture
displayShootHere ((x1,y1),(x2,y2)) = translate (x2 + 0.05 * screenDivider) (y1 + 0.2 * screenDivider) $ pictures [scale sc sc $ text s]
                                                    where sc = screenDivider / 1400
                                                          s = "Shoot here ---->"                                                          

displayGameStage :: GameStage -> BoardPos -> Picture
displayGameStage stage ((x1,y1),(x2,y2)) = translate (x2 + 0.1 * screenDivider) (y2 - 0.4 * screenDivider) $ pictures [scale sc sc $ text s]
                                                    where sc = screenDivider / 1100
                                                          s = case stage of
                                                              Placing User  -> "Place ships!"
                                                              Shooting User -> "Shoot enemy!"
                                                       
 
displayWinner :: Maybe Player -> BoardPos -> Picture
displayWinner Nothing _ = Blank
displayWinner player ((x1,y1),(x2,y2)) = translate (x2 + 0.1 * screenDivider) (y2 - 0.4* screenDivider) $ pictures [scale sc sc $ text s]
                                         where sc = screenDivider / 1100
                                               s = case player of
                                                   Just User -> "You won!"
                                                   Just AI   -> "You lost!"

displayInstructions :: GameStage -> BoardPos -> Picture
displayInstructions stage ((x1,y1),(x2,y2)) = pictures [translate (x2 + 0.1 * screenDivider) (y2 - 1.2 * screenDivider) $ pictures [scale sc sc $ text s1],
                                                        translate (x2 + 0.1 * screenDivider) (y2 - 1.3 * screenDivider) $ pictures [scale sc sc $ text s2],
                                                        translate (x2 + 0.1 * screenDivider) (y2 - 1.4 * screenDivider) $ pictures [scale sc sc $ text s3 ]]
                                                        where sc = screenDivider / 2200
                                                              (s1, s2, s3) = case stage of
                                                                             Placing User -> ("Move ship with arrow keys,", "rotate with r and ", "confirm placement with enter")
                                                                             Shooting User -> ("Click on a cell on", "the enemy's board to shoot","")

displayRestartInstructions :: BoardPos -> Picture
displayRestartInstructions ((x1,y1),(x2,y2)) = pictures [translate (x2 + 0.1 * screenDivider) (y2 - screenDivider) $ pictures [scale sc sc $ text s1], 
                                                         translate (x2 + 0.1 * screenDivider) (y2 - 1.1 * screenDivider) $ pictures [scale sc sc $ text s2],  
                                                         translate (x2 + 0.1 * screenDivider) (y2 - 1.2 * screenDivider) $ pictures [scale sc sc $ text s3]]
                                               where sc = screenDivider / 2200
                                                     (s1, s2, s3) = ("Left mouse button to", "play next round,", "escape to quit")
                                                     
                                                     
displayCurrentRound :: Int -> BoardPos -> Picture
displayCurrentRound round ((x1,y1),(x2,y2)) = translate (x2 + 0.1 * screenDivider) (y2 - 0.55 * screenDivider) $ pictures [scale sc sc $ text $ "Round " ++ show round]
                                                        where sc = screenDivider / 1100

displayStats :: ((Player, Int), (Player, Int)) -> BoardPos -> Picture
displayStats ((user, n1), (ai, n2)) ((x1,y1),(x2,y2)) = pictures [translate (x2 + 0.1 * screenDivider) (y2 - 0.7 * screenDivider) $ pictures [scale sc sc $ text $ "User wins: " ++ show n1], 
                                                                  translate (x2 + 0.1 * screenDivider) (y2 - 0.85 * screenDivider) $ pictures [scale sc sc $ text $ "AI wins: " ++ show n2]]
                                                        where sc = screenDivider / 1100
                                                              

missToPicture :: Board -> BoardPos -> Picture
missToPicture board pos = cellsToPicture board pos (Empty Checked) crossPicture

hitsToPicture :: Board -> BoardPos -> Picture
hitsToPicture board pos = cellsToPicture board pos (Ship Checked) shipPicture

shipsToPicture :: Board -> BoardPos -> Picture
shipsToPicture board pos = cellsToPicture board pos (Ship NotChecked) shipPicture

showPlacingShip :: Ships -> Picture
showPlacingShip [] = Blank
showPlacingShip ((coord, d, s): xs) = movingShipPicture coord d s 

gameAsRunningPicture :: Game -> Picture
gameAsRunningPicture game =
    pictures [color radarColor $ radarPicture radar,
              color boardGridColor boardAIGrid,
              color boardGridColor boardUserGrid,
              color green $ displayGameName boardUserPos,
              color boardGridColor $
              color missColor $ missToPicture userBoard boardUserPos,
              if win == Nothing then color white $ displayGameStage stage boardUserPos else Blank,
              if win == Nothing then Blank else color white $ displayRestartInstructions boardUserPos,
              if win == Nothing then color white $ displayInstructions stage boardUserPos else Blank,
              if stage == Placing User then color white $ displayPlaceHere boardUserPos else color white $ displayShootHere boardUserPos,
              color textColor $ displayCurrentRound currRound boardUserPos,
              color textColor $ displayWinner win boardUserPos,
              color textColor $ displayStats winStats boardUserPos,
              color missColor $ missToPicture boardAI boardAIPos,
              color shipColor $ shipsToPicture userBoard boardUserPos,
              color hitColor  $ hitsToPicture userBoard boardUserPos,
              color hitColor  $ hitsToPicture boardAI boardAIPos,
              color movShipColor  $ showPlacingShip ships,
              color red $ moveExplosion r pos b

             ]
             where userBoard = gameBoardUser game
                   boardAI = gameBoardAI game
                   ships = shipsUser game
                   stage = gameStage game
                   win = winner game
                   currRound = currentRound game
                   winStats = stats game
                   (r, pos,_, _,b) =  shootAnimation game
                   radar = radarAnimation game



drawGame :: Game -> Picture
drawGame game = translate (screenWidth * (-0.5))
                          (screenHeight * (-0.5))
                           frame
        where frame = gameAsRunningPicture game