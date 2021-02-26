

{-

INITIAL DRAFT OF SHOOTING AI WHICH CALCULATES POSSIBLE POSITIONS OF ENEMY SHIPS 
REGARDING WHAT SHIPS ARE LEFT TO SHOOT AND VALID LOCATIONS

-}

{- findSunkenShips 
    finds the sizes of sunken
    -- iterate through board for hits (Ships Checked)
                 miss hit .. hit miss -> length of hits = length of ship
-}

{- findValidPlacements board shipSizes 
    generates a list of valid placements for ALL unsunken ships

-}


type Placement = (CellCoord, Direction)

findValidPlacements :: Board -> [ShipSize] -> [Placement]

{- findCohesiveCells coord board
    RETURNS: cohesive cells to coord on board
-}
findCohesiveCells :: CellCoord -> Board -> [CellCoord]




{- nextMove board stack shootingList
    returns the move of the ai, updated stack, updated shootList. 
-}

nextMove :: Board -> Stack -> [CellCoord] -> (CellCoord, Stack, [CellCoord])
nextMove board stack shootingList
    | stack.isEmpty = (head shootingList, stack, tail shootingList) -- head/random
    | otherwise = (first, rest, shootingList) 
        where (first, rest) = stack.pop

{- moveAI board stack shootingList
    returns the move of the ai, updated stack, updated shootList. 

-}

moveAI :: Board -> Stack -> [CellCoord] -> (CellCoord, Stack, [CellCoord])
moveAI board stack shootingList
            | move == hit {- Ship NotChecked -} = (move, filteredStack, filteredShootingList)
            | otherwise = (move, stack, shootingList)
            where
                (move, stack, shootingList) = nextMove board stack shootingList
                cohesiveCells = findCohesiveCells
                possiblePositions = findValidPlacements board shipSizes
                filteredStack = filter (==possiblePositions) stack.push(cohesiveCells) 
                filteredShootingList = filter (==possiblePositions) shootingList.remove(cohesiveCells)

{-
board
shipSizes
stack
shootingList
-}
