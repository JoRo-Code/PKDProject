module Animation where

import Game


{- animationFunc time game
    updates radius of shootanimation according to time
    also makes sure to change from show to hidden when outside of boundary
-}
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

