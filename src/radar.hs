import Graphics.Gloss


screenWidth :: Float
screenWidth = 1500
screenHeight :: Float
screenHeight = 700

window = InWindow "BattleShips" (floor screenWidth, floor screenHeight) (100, 100)

backgroundColor = white

radarColor = makeColorI 0 100 0 255
thickness = 5

type Radius = Float

-- r of particles
type Radar = (Radius, Radius, Radius, Radius, Radius)
    
radar = (1, 2, 3, 4, 5)

eventHandler _ game = game

squarePicture size = rectangleSolid (size * screenHeight) (size * screenHeight)


radarPicture (r1, r2, r3, r4, r5) =     
    translate (screenWidth/2) (screenHeight/2) pictures 
    [
     thickCircle r1 5, 
     thickCircle r2 5,
     thickCircle r3 5, 
     thickCircle r4 5,
     thickCircle r5 5
    ] 

drawFunc game = 
    pictures 

    [
    color radarColor $ radarPicture radar
    ]
    where radar = radarAnimation game

updateFunc :: Float -> World -> World
updateFunc dt game  =  game (newRadius r1, newRadius r2, newRadius r3, newRadius r4, newRadius r5)
            where newRadius r 
                            | abs r >= screenWidth = 0
                            | otherwise = r + 0.1 + 0.5 * r*dt + r*dt 

main = play window backgroundColor 30 initGame drawFunc eventHandler updateFunc



{-
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Game

screenWidth :: Float
screenWidth = 1500
screenHeight :: Float
screenHeight = 700

window = InWindow "BattleShips" (floor screenWidth, floor screenHeight) (100, 100)

backgroundColor = white

radarColor = makeColorI 0 100 0 255
thickness = 5

middlePos = (screenWidth/2, screenHeight/2)

startRadius = 0
startDerivative = screenWidth/2

type Radius = Float

-- position on screen
type Pos = (Float, Float)

type Derivative = Float

-- radius, position of centre of wave, end radius of wave, derivative, showBool.
data Game = Game { shootAnimation :: (Radius, Pos, Radius, Derivative, Bool)}
    
initGame = Game { 
    shootAnimation = (startRadius, middlePos, screenHeight/2, startDerivative, False)
    } 


-- updating pos
eventHandler (EventKey (MouseButton LeftButton) Up _ mousePos) game = game {shootAnimation = (startRadius, mousePos, end, startDerivative, True)}
    where (_,_, end, _, _) = shootAnimation game

eventHandler _ game = game


explosionPicture :: Radius -> Picture
explosionPicture r = thickCircle r thickness

moveExplosion :: Radius -> Pos -> Bool -> Picture 
moveExplosion _ _ False = Blank
moveExplosion r (x,y) _ = translate x y (explosionPicture r)

-- rendering
gameAsRunningPicture :: Game -> Picture
gameAsRunningPicture game = 

    pictures 
    [
    color radarColor $ moveExplosion r pos b

    ]
    where (r, pos,_, _,b) =  shootAnimation game

drawGame :: Game -> Picture
drawGame game = {-translate (screenWidth * (-0.5))
                          (screenHeight * (-0.5))-}
                           frame
        where frame = gameAsRunningPicture game

-- updating radius
animationFunc :: Float -> Game -> Game
animationFunc dt game  = game {shootAnimation = (newRadius r1 d, pos, end, newD d, showExplosion r1 b)} 
            where 
                (r1, pos, end, d, b) = shootAnimation game 
                newD d = d*0.95
                newRadius r d
                            | abs r >= end = 0
                            | otherwise = r + d*dt
                showExplosion r b 
                                | abs r >= end = False
                                | otherwise = b 

main = play window backgroundColor 30 initGame drawGame eventHandler animationFunc

-}