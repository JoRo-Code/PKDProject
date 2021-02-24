module PlacingAI where

import System.Random


allCoords :: [CellCoord]
allCoords = [(c, r) | c <- [0..n-1], r <- [0..n-1]]


findValidDirectionalPlacements :: Board -> [CellCoord] -> ShipSize ->  Direction -> [(CellCoord, Direction)]
findValidDirectionalPlacements b coords s d = map (\coord -> (coord, d)) $ filter (\coord -> validShipPlacement b coord s d) coords
                 

findAllValidPlacements :: Board -> ShipSize -> [(CellCoord, Direction)]
findAllValidPlacements b s = findValidDirectionalPlacements b allCoords s Horizontal ++ findValidDirectionalPlacements b allCoords s Vertical 


randomPlacement :: StdGen ->  [(CellCoord, Direction)]  -> ((CellCoord, Direction), StdGen)
randomPlacement gen list = (list !! randomInt, newGen)
                     where range = (0, length list - 1)
                           (randomInt, newGen) = randomR range gen

placeShipAI :: StdGen -> Board -> ShipSize -> [(CellCoord, Direction)] -> (Board, StdGen)
placeShipAI gen b s placements = (placeShipAux b coord s d, newGen)
                             where ((coord , d), newGen) = randomPlacement gen placements --randomPlacement placements -- call random function here


placeMultipleShipsAI :: StdGen -> Board -> Ships -> Board
placeMultipleShipsAI gen b [] = b
placeMultipleShipsAI gen b ((_, _, s):ships) = placeMultipleShipsAI newGen newBoard ships
                                      where (newBoard, newGen) = placeShipAI gen b s (findAllValidPlacements b s)


--

validShipPlacement :: Board ->  CellCoord -> ShipSize -> Direction -> Bool
validShipPlacement b (c, r) s d = validCoordinates (endCoordinates (c, r) s d) -- (-1) because the ship part on (r, c) is included
                                  && validCoordinates (c, r)  
                                  && followPlacementRules b (c,r) s d

validCoordinates :: CellCoord -> Bool
validCoordinates  = inRange boardIndex
                    where boardIndex = ((0, 0), (n - 1, n - 1)) 

endCoordinates :: CellCoord -> ShipSize -> Direction -> CellCoord
endCoordinates (c, r) s Horizontal = (c + s - 1, r)
endCoordinates (c, r) s Vertical = (c, r - s + 1)