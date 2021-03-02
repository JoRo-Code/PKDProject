module Rendering where

import Graphics.Gloss
import Data.Array
import Game

-- Colors of objects
boardGridColor :: Color
boardGridColor = makeColorI 0 100 0 255
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



radarThickness :: Float
radarThickness = 3



---------------------------- Pictures ----------------------------

{- crossPicture
    creates a cross with respect to grid-size
    Credit to Tsoding. Modified the function to be flexible with grid-size
    https://github.com/tsoding/profun/blob/master/functional/src/Rendering.hs
    RETURNS: picture of a cross proportional to mininum of cellWidth and cellHeight
    EXAMPLES: 
                crossPicture == Pictures [Rotate 45.0 (Polygon [(-19.949999,-4.275),(-19.949999,4.275),(19.949999,4.275),(19.949999,-4.275)]),Rotate (-45.0) (Polygon [(-19.949999,-4.275),(-19.949999,4.275),(19.949999,4.275),(19.949999,-4.275)])]
                             -> equilateral cross with cross-element corresponding to 70% of the minumum of cellWidth and cellHeight

-}
crossPicture :: Picture
crossPicture  = pictures [ rotate 45 $ rectangleSolid length thickness
                 , rotate (-45) $ rectangleSolid length thickness
                 ]
                 where length = 0.7 * min cellWidth cellHeight
                       thickness = 0.15 * min cellWidth cellHeight

{- shipPicture
    creates block proportional to cellsizes
    RETURNS: picture of filled rectangle proportional to cellWidth and cellHeight
    EXAMPLES:   
                shipPicture == Pictures [Polygon [(-19.949999,-19.949999),(-19.949999,19.949999),(19.949999,19.949999),(19.949999,-19.949999)]]
                            -> rectangle with 70% of cellWidth as width and 70% of cellHeight as height


-}
shipPicture :: Picture
shipPicture = pictures [ rectangleSolid (0.7 * cellWidth) (0.7 * cellHeight)]

{- boardGrid boardpos
    creates a picture of a grid at boardpos
    Credit to Tsoding. Modified the function to be flexible with multiple grids
    https://github.com/tsoding/profun/blob/master/functional/src/Rendering.hs
    RETURNS: picture of grid at boardpos with global n rows and n columns.
    EXAMPLES: 
                boardGrid ((0,0), (screenWidth/2, screenHeight/2))                             -> n^2-sized grid positioned in the first quadrant
                boardGrid ((-screenWidth/2, -screenHeight/2), (screenWidth/2, screenHeight/2)) -> n^2-sized grid starting from bottom left and finishing in the upper right corner
    
-}
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

{- movingShipPicture coord direction shipsize
    displays moving ship with correct length and direction at coord
    PRE: shipsize >= 0, valid coord
    RETURNS: picture of a rectangle of shipsize beginning at coord in direction.
    EXAMPLES:
                movingShipPicture (0,0) Horizontal 3    == Translate 85.5 28.5 (Pictures [Polygon [(-85.5,-28.5),(-85.5,28.5),(85.5,28.5),(85.5,-28.5)]])
                                                        -> 3 cells long rectangle laying horizontally to the right from (0,0)
-}
movingShipPicture :: CellCoord -> Direction -> ShipSize -> Picture
movingShipPicture (c, r) Horizontal s =  translate (cellWidth * (0.5 * fromIntegral s + fromIntegral c)) (cellHeight * (0.5 + fromIntegral r)) 
                                          (pictures [ rectangleSolid (cellWidth * fromIntegral s) cellHeight])
movingShipPicture (c, r) Vertical s   =  translate (cellWidth * (fromIntegral c + 0.5)) (cellHeight * (1 + fromIntegral r - 0.5 * fromIntegral s)) 
                                          (pictures [ rotate 90 $ rectangleSolid (cellWidth * fromIntegral s) cellHeight])                        

{- showPlacingShip ships
    transforms head of ships into a moving picture
    PRE: shipsize of ships >= 0, valid coord
    RETURNS: picture representation of head of ships, Blank if ships is empty.
    EXAMPLES:
                showPlacingShips []                         -> Blank
                showPlacingShips ((0,0)), Horizontal, 3)    -> 3 cells long rectangle laying horizontally to the right from (0,0) 
                showPlacingShips ((5,5), Vertical, 2)       -> 2 cells long rectangle laying vertically downways from (5,5)  

-}
showPlacingShip :: Ships -> Picture
showPlacingShip [] = Blank
showPlacingShip ((coord, d, s): xs) = movingShipPicture coord d s 


{- explosionPicture r
    explosion with radius r
    RETURNS: picture of circle with radius r
    EXAMPLES: 
                explosionPicture 10     == ThickCircle 10.0 10.0
                explosionPicture 50     == ThickCircle 50.0 10.0
                explosionPicture (-1)   == ThickCircle (-1.0) 10.0
-}
explosionPicture :: Radius -> Picture
explosionPicture r = thickCircle r 10


{- fadedArc ang r
    creates a faded arc picture with angle and r
    PRE: angle >= 0
    RETURNS: arc with angle ang and radius r with a green gradient with ang sections. 
    EXAMPLES: 
                fadedArc 45 50 -> 45 angle arc of radius 50 with a green gradient
                fadedArc 0 50  ->  Blank

-}
fadedArc :: Int -> Float -> Picture
fadedArc angle r = 
    pictures
    $ concatMap (\i -> [color  (makeColorI 0 (i*255 `div` angle) 0 255)  $ rotate (fromIntegral i) $ arcSolid 0 1 r]
    )
    [0 .. angle]


{- radarPicture (radiuses, angle) boardpos
    creates spinning radar with circles
    PRE: radiuses not empty
    RETURNS: radar arc with max radiuses as radius, beginning at middle of boardpos, rotated at angle, enclosed by circles of radiuses.
    EXAMPLES:
                radarPicture ([1..5], 45) boardAIPos -> radar arc with radius 5 beginning at middle of boardAIPos rotated at 45 degrees enclosed by circles of radiuses 1,2,3,4,5
                radarPicture ([1], 45) boardAIPos -> radar arc with radius 1 beginning at middle of boardAIPos rotated at 45 degrees enclosed by circles of radiuses 1,2,3,4,5

-}
radarPicture :: Radar -> BoardPos ->  Picture
radarPicture (radiuses, angle) ((x1,y1),(x2,y2)) =     
    translate (0.5 * (x1 + x2)) (0.5 * (y1 + y2)) 
    (pictures 
    [
     rotate angle $ fadedArc 90 maxRadius
     , rotate 45.0 angleLine, rotate (-45.0) angleLine
     , pictures $ concatMap (\radius -> [thickCircle radius radarThickness]) radiuses
    ]) 
    where angleLine = line [(-maxRadius,0) ,(maxRadius, 0)]
          maxRadius = maximum radiuses


---------------------------- Info to user ----------------------------


combineDisplayText :: BoardPos -> GameStage -> Maybe Player -> Round -> Stats -> Picture
combineDisplayText pos stage winner round stats = pictures [if winner == Nothing then displayGameStage pos stage else Blank, displayWinner pos winner, 
                                                            if winner == Nothing then displayInstructions pos stage else Blank, 
                                                            if winner == Nothing then Blank else displayRestartInstructions pos,
                                                            if stage == Placing User then displayArrowInstruction pos "<---- Place here" 
                                                            else displayArrowInstruction pos "Shoot here ---->",
                                                            displayCurrentRound pos round, displayStats pos stats ]
displayGameName :: BoardPos -> Picture
displayGameName ((x1,y1),(x2,y2)) = translate (x2 + 0.05 * screenDivider) (y2 - 0.2 * screenDivider) $ pictures [scale sc sc $ text s]
                                                    where sc = screenDivider / 900
                                                          s = "BATTLESHIPS"

displayArrowInstruction  :: BoardPos -> String -> Picture
displayArrowInstruction ((x1,y1),(x2,y2)) s = translate (x2 + 0.05 * screenDivider) (y1 + 0.2 * screenDivider) $ pictures [scale sc sc $ text s]
                                                    where sc = screenDivider / 1400
                                                  

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


{- snapPictureTocell pic boardpos coord
    puts a picture to a specific cell's screencoordinates.
    Credit to Tsoding. Modified the function to be flexible with multiple grids
    https://github.com/tsoding/profun/blob/master/functional/src/Rendering.hs

    RETURNS: pic positioned at coord in grid by boardpos 
 -} 
snapPictureToCell :: Picture -> BoardPos -> CellCoord -> Picture
snapPictureToCell picture boardPos@((x1,y1),(x2,y2)) (c, r) = translate x y picture
    where x = x1 + fromIntegral c * cellWidth + cellWidth / 2
          y = y1 + fromIntegral r * cellHeight + cellHeight / 2
          


{- cellsToPicture

-}
cellsToPicture :: Board -> BoardPos -> Cell -> Picture -> Picture
cellsToPicture board pos c pic =  pictures
                            $ map (snapPictureToCell pic pos . fst)
                            $ filter (\(_, e) -> e == c)
                            $ assocs board                                                              

{- displayCells 

-}
displayCells :: Board -> BoardPos -> Bool -> Picture
displayCells board pos show = pictures 
                                [color missColor $ cellsToPicture board pos (Empty Checked) crossPicture
                               , color hitColor  $ cellsToPicture board pos (Ship Checked) shipPicture
                               , if show then color shipColor $ cellsToPicture board pos (Ship NotChecked) shipPicture else Blank
                                ]

{- moveExplosion r pos show
    RETURNS: 


-}
moveExplosion :: Radius -> ScreenCoord -> Bool -> Picture 
moveExplosion _ _ False = Blank
moveExplosion r (x,y) _ = translate (screenWidth/2) (screenHeight/2) (translate x y (explosionPicture r))


{- gameToPicture game
    turns game into a picture with lower left corner in the middle of the screen
    RETURNS: a picture representation of game with lower left corner in the middle of the screen 
-}
gameToPicture :: Game -> Picture
gameToPicture game =
    pictures [color radarColor $ pictures [radarPicture radar boardUserPos, radarPicture radar boardAIPos],
              color boardGridColor $ pictures [boardGrid boardUserPos, boardGrid boardAIPos],
              color green $ displayGameName boardUserPos,
              color textColor $ combineDisplayText boardUserPos stage win currRound winStats,
              pictures [displayCells userBoard boardUserPos True , displayCells boardAI boardAIPos False],
              color movingShipColor  $ showPlacingShip ships,
              color (if ishit then red else cyan) $ moveExplosion r pos b
             ]
             where userBoard = gameBoardUser game
                   boardAI = gameBoardAI game
                   ships = shipsUser game
                   stage = gameStage game
                   win = winner game
                   currRound = currentRound game
                   winStats = stats game
                   (ishit, r, pos,_, _,b) =  shootAnimation game
                   radar = radarAnimation game


{- drawGame game 
    transforms game into a picture
    RETURNS: picture representation of game with lower left corner in the lower left of the screen. 
-}
drawGame :: Game -> Picture
drawGame game = translate (screenWidth * (-0.5))
                          (screenHeight * (-0.5))
                           picture
        where picture = gameToPicture game