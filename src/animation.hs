module Animation where

import Game


{- animationFunc time game
    updates radius of shootanimation according to time
    also makes sure to change from show to hidden when outside of boundary
-}
animationFunc :: Float -> Game -> Game
animationFunc dt game  = game {shootAnimation = (newRadiusExplosion r d, pos, end, newD d, showExplosion r b),
                               radarAnimation = (newRadius r1, newRadius r2, newRadius r3, newRadius r4, newRadius r5, newRadius r5)
                                } 
            where
                (r, pos, end, d, b) = shootAnimation game 
                (r1, r2, r3, r4, r5, angle) = radarAnimation game
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

