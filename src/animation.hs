module Animation where

import Game


{- animationFunc time game
    updates shooting and radar animation with respect to time.
    RETURNS: game with new angle of radarAnimation, and new radius and derivative for shootingAnimation and whether or not shootingAnimation should be shown with respect to time
    EXAMPLES:
                    increasing radius               -> shootAnimation explodes
                    decreasing derivative of radius -> shootAnimation fades out
                    showBool                        -> hides explosion at end radius

                    increasing angle of radius      -> spinning radar


                
                
                
-}
animationFunc :: Float -> Game -> Game
animationFunc dt game  = game {shootAnimation = (hitShip, newRadius r d, pos, end, newD d, showExplosion r b),
                               radarAnimation = (radiuses, newAngle angle)
                                } 
            where
                (hitShip, r, pos, end, d, b) = shootAnimation game 
                (radiuses, angle) = radarAnimation game

                --  derivative of shootAnimation decreases with time -> fades explosion
                newD d = d*0.95

                --  radius of shootAnimation increases with respect to derivative -> explodes
                newRadius r d
                            | abs r >= end = 0
                            | otherwise = r + d*dt

                --  shootAnimation gets hidden if it has reached its end boundary -> stops showing
                showExplosion r b 
                                | abs r >= end = False
                                | otherwise = b

                --  angle of radarAnimation increases with time -> spins
                newAngle angle | angle == 360.0 = 0.0
                               | otherwise = angle + 4.0

