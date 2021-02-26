module Rendering where

import Graphics.Gloss
import Data.Array
import Game

boardGridColor = makeColorI 0 100 0 255

type BoardPos = (ScreenCoord, ScreenCoord)

textColor :: Color
textColor = white
missColor :: Color
missColor = white
hitColor :: Color
hitColor = red
shipColor :: Color
shipColor = makeColorI 128 128 128 220
movingShipColor :: Color
movingShipColor = makeColorI 128 128 128 200
radarColor :: Color
radarColor = makeColorI 0 140 0 255

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

radarPicture :: Radar -> BoardPos ->  Picture
radarPicture (radiuses, angle) ((x1,y1),(x2,y2)) =     
    translate (0.5 * (x1 + x2)) (0.5 * (y1 + y2)) 
    (pictures 
    [
     rotate angle $ fadedArc 90 maxRadius green ,
     rotate 45.0 angleLine, rotate (-45.0) angleLine,
     pictures $ concatMap (\radius -> [thickCircle radius radarThickness]) radiuses
    ]) 
    where angleLine = line [(-maxRadius,0) ,(maxRadius, 0)]
          maxRadius = maximum radiuses


{- fadedArc angle r color
    creates a faded arc picture with angle and r of color
-}

fadedArc :: Int -> Float -> Color -> Picture
fadedArc angle r c = 
    pictures
    $ concatMap (\i -> [color  (makeColorI 0 (i*255 `div` angle) 0 255)  $ rotate (fromIntegral i) $ arcSolid 0 1 r]
    )
    [0 .. angle]



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


combineDisplayText :: BoardPos -> GameStage -> Maybe Player -> Round -> Stats -> Picture
combineDisplayText pos stage winner round stats = pictures [if winner == Nothing then displayGameStage pos stage else Blank, displayWinner pos winner, 
                                                            if winner == Nothing then displayInstructions pos stage else Blank, 
                                                            if winner == Nothing then Blank else displayRestartInstructions pos,
                                                            if stage == Placing User then displayPlaceHere pos else displayShootHere pos,
                                                            displayCurrentRound pos round, displayStats pos stats ]
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

displayGameStage :: BoardPos -> GameStage -> Picture
displayGameStage ((x1,y1),(x2,y2)) stage = translate (x2 + 0.1 * screenDivider) (y2 - 0.4 * screenDivider) $ pictures [scale sc sc $ text s]
                                                    where sc = screenDivider / 1100
                                                          s = case stage of
                                                              Placing User  -> "Place ships!"
                                                              Shooting User -> "Shoot enemy!"

displayWinner :: BoardPos -> Maybe Player-> Picture
displayWinner  _ Nothing = Blank
displayWinner ((x1,y1),(x2,y2)) player  = translate (x2 + 0.1 * screenDivider) (y2 - 0.4* screenDivider) $ pictures [scale sc sc $ text s]
                                         where sc = screenDivider / 1100
                                               s = case player of
                                                   Just User -> "You won!"
                                                   Just AI   -> "You lost!"



displayInstructions :: BoardPos -> GameStage -> Picture
displayInstructions ((x1,y1),(x2,y2)) stage = pictures $ concatMap  
                                               (\i -> [translate xTranslate (y2 - (1.2 + 0.1 * fromIntegral i) * screenDivider) 
                                               $ scale sc sc $ text $ strings !! i]) 
                                               [0..length strings - 1]
                                               where sc = screenDivider / 2200
                                                     strings =  case stage of
                                                                             Placing User -> ["Move ship with arrow keys,", "rotate with r and ", "confirm placement with enter"]
                                                                             Shooting User -> ["Click on a cell on", "the enemy's board to shoot"]
                                                     xTranslate = x2 + 0.1 * screenDivider  

displayRestartInstructions :: BoardPos -> Picture
displayRestartInstructions ((x1,y1),(x2,y2)) = pictures $ concatMap  
                                               (\i -> [translate xTranslate (y2 - (1 + 0.1 * fromIntegral i) * screenDivider) 
                                               $ scale sc sc $ text $ strings !! i]) 
                                               [0..length strings - 1]
                                               where sc = screenDivider / 2200
                                                     strings = ["Left mouse button to", "play next round,", "escape to quit"]
                                                     xTranslate = x2 + 0.1 * screenDivider                                                     
                                                     
displayCurrentRound :: BoardPos -> Round -> Picture
displayCurrentRound ((x1,y1),(x2,y2)) round = translate (x2 + 0.1 * screenDivider) (y2 - 0.55 * screenDivider) $ pictures [scale sc sc $ text $ "Round " ++ show round]
                                                        where sc = screenDivider / 1100

displayStats :: BoardPos -> Stats -> Picture
displayStats ((x1,y1),(x2,y2)) ((user, n1), (ai, n2)) = pictures [translate xTranslate (y2 - 0.7 * screenDivider) $ pic "User wins: " n1, 
                                                                  translate xTranslate (y2 - 0.85 * screenDivider) $ pic "AI wins: " n2]
                                                        where sc = screenDivider / 1100
                                                              xTranslate = x2 + 0.1 * screenDivider
                                                              pic s stat = pictures [scale sc sc $ text $ s ++ show stat]
                                                              
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
    pictures [color radarColor $ pictures [ radarPicture radar boardUserPos, radarPicture radar boardAIPos],
              color boardGridColor $ pictures [boardAIGrid, boardUserGrid],
              color green $ displayGameName boardUserPos,
              color textColor $ combineDisplayText boardUserPos stage win currRound winStats,
              color missColor $ pictures [missToPicture userBoard boardUserPos, missToPicture boardAI boardAIPos],
              color hitColor  $ pictures [hitsToPicture userBoard boardUserPos, hitsToPicture boardAI boardAIPos],
              color shipColor $ shipsToPicture userBoard boardUserPos,
              color movingShipColor  $ showPlacingShip ships,
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