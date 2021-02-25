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
type World = (Radius, Radius, Radius, Radius, Radius)
    
initGame = (1, 2, 3, 4, 5)

eventHandler _ game = game


drawFunc (r1, r2, r3, r4, r5)  = 
    pictures 
    [
    color radarColor $ thickCircle r1 thickness, 
    color radarColor $ thickCircle r2 thickness,
    color radarColor $ thickCircle r3 thickness, 
    color radarColor $ thickCircle r4 thickness,
    color radarColor $ thickCircle r5 thickness
    ]

updateFunc :: Float -> World -> World
updateFunc dt (r1, r2, r3, r4, r5)  = (newRadius r1, newRadius r2, newRadius r3, newRadius r4, newRadius r5)
            where newRadius r 
                            | abs r >= screenWidth = 0
                            | otherwise = r + 0.1 + 0.5 * r*dt + r*dt 

main = play window backgroundColor 30 initGame drawFunc eventHandler updateFunc
