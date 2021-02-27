module Animation where

import Game


{- animationFunc time game
    updates radius of shootanimation according to time
    also makes sure to change from show to hidden when outside of boundary
-}
animationFunc :: Float -> Game -> Game
animationFunc dt game  = game {shootAnimation = (hitShip, newRadiusExplosion r d, pos, end, newD d, showExplosion r b),
                               radarAnimation = (radiuses, newAngle angle)
                                } 
            where
                (hitShip, r, pos, end, d, b) = shootAnimation game 
                (radiuses, angle) = radarAnimation game
                newD d = d*0.95
                newRadiusExplosion r d
                            | abs r >= end = 0
                            | otherwise = r + d*dt
                showExplosion r b 
                                | abs r >= end = False
                                | otherwise = b
                newRadius r 
                            | abs r >= screenWidth = 0
                            | otherwise = r + 0.1 + 0.5 * r*dt + r*dt
                newAngle angle | angle == 360.0 = 0.0
                               | otherwise = angle + 4.0

