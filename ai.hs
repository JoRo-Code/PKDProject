{-# LANGUAGE LambdaCase #-} 

module AI where




------------------- Imports -------------------
import Data.Array
import System.IO
import System.Random
import Data.List

--import Data.Stack
------------------- Imports -------------------
data SquareState = Checked           | NotChecked                 deriving (Show, Eq)
data Cell        = Empty SquareState | Ship SquareState           deriving (Show, Eq)
data Player      = User              | AI                         deriving (Show, Eq)
data GameStage   = Placing Player    | Shooting Player            deriving (Show, Eq)
data Direction   = Horizontal        | Vertical                   deriving (Show, Eq)  

type Row         = Int
type Col         = Int
type CellCoord   = (Col, Row)
type ScreenCoord = (Float, Float)
type Board       = Array (Col, Row) Cell
type BoardSize   = Int
type ShipSize    = Int

type ShootList = [(CellCoord,Cell)]
type Stack = [(CellCoord,Cell)]
type CheckList = [(CellCoord,Cell)] -- cells who have been checked or do not have to be checked

{- shuffle list seed
   shuffles a list with a seed
-}
shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle [] gen = ([], gen)
shuffle list gen = shuffleAux list [] gen


{- shuffleAux list newList seed
    newly randomly generated newList of list with seed
-}

shuffleAux :: [a] -> [a] -> StdGen -> ([a], StdGen)
-- VARIANT: length list
shuffleAux [] shuffledList gen = (shuffledList, gen)
shuffleAux list randomList gen = shuffleAux updatedList (pickedElement:randomList) finalGen
        where
            range = (0, length list-1)
            (randomIndex, finalGen) = randomR range gen :: (Int, StdGen)
            pickedElement = list !! randomIndex
            updatedList = removeIndex list randomIndex



{- removeIndex list index 
    RETURNS: a list with the element at position index removed
-}
removeIndex :: [b] -> Int -> [b]
removeIndex list index = map snd $ filter (\(i, element) -> i /= index) (zip [0..length list - 1] list)
-- returns boardsize from given board

validCoordinates :: CellCoord -> Bool
validCoordinates  = inRange boardIndex
                    where boardIndex = ((0, 0), (n - 1, n - 1)) 

getCell :: Board -> CellCoord -> Cell
getCell b c = b ! c

checkCell :: Board -> CellCoord -> Board
checkCell b (c, r) = case getCell b (c, r) of
                      Empty NotChecked -> b // [((c, r), Empty Checked)]   
                      Ship NotChecked  -> b // [((c, r), Ship Checked)]
                      _ -> b

initBoard :: Board        
initBoard = array boardIndex $ zip (range boardIndex) (repeat $ Empty NotChecked)
             where boardIndex = ((0, 0), (n - 1, n - 1)) 

n :: BoardSize
n = 4
--------------------- AI --------------------------------

-- Returns column of element in a ShootList
getCol :: (CellCoord,Cell) -> Col
getCol ((a,_),_) = a

-- Returns row of element in a ShootList
getRow :: (CellCoord,Cell) -> Row
getRow ((_,b),_) = b

aiShootList :: Board -> CheckList -> ShootList
aiShootList b cl = removeShootList cl [((c,r), b ! (c,r)) | c <- [0..n-1], r <- [0..n-1]]


-- Sorts a shootlist so AI prioritises cells that are not next to each other
aiPrio :: ShootList -> ShootList -> ShootList
aiPrio [] acc = acc
aiPrio (x:xs) acc 
  | even $ getCol x = if  odd $ getRow x then aiPrio xs ([x] ++ acc) else aiPrio xs (acc ++ [x])
  | otherwise       = if even $ getRow x then aiPrio xs ([x] ++ acc) else aiPrio xs (acc ++ [x])

aiPrio' :: ShootList -> ShootList
aiPrio' s = foldl combos [] s

combos :: ShootList -> (CellCoord,Cell) -> ShootList
combos acc e@((c, r), cell) = case (even c, even r) of
                              (True, False)  -> e : acc
                              (True, True)   -> acc ++ [e]
                              (False, True)  -> e : acc
                              (False, False) -> acc ++ [e]

-- Removes already checked cells from Stack or ShootList
removeChecked :: [(CellCoord, Cell)] -> [(CellCoord, Cell)]
removeChecked s = filter (\(coord, cell) -> cell == Empty NotChecked || cell == Ship NotChecked) s

-- Creates a prioritised ShootList of all cells with state NotChecked
filterShootList :: Board -> CheckList -> StdGen -> (ShootList, StdGen)
filterShootList b cl gen = (removeShootList cl (removeChecked (aiPrio shuffledList [])), newGen)
    where (shuffledList, newGen) = shuffle (aiShootList b cl) gen 

-- Updates Stack with the head from ShootList if Stack is empty
updateStack :: Stack -> ShootList -> Stack
updateStack [] (x:xs) = [x]
updateStack s _ = s

-- Returns a Stack of the cohesive cells to a cell in a row
cohesiveCells :: Board -> (CellCoord,Cell) -> [(CellCoord,Cell)]
cohesiveCells b ((c,r),x) = map (\coord -> (coord, b ! coord)) (filter validCoordinates [(c,r+1), (c,r-1), (c-1,r), (c+1,r)])

-- Returns True if a Cell contains an unchecked Ship, otherwise False
isShip :: (CellCoord,Cell) -> Bool
isShip (_, Ship NotChecked) = True
isShip _ = False

-- Checks first cell in stack
--PRE: ShootList is not empty
aiShootAux :: (Board, Stack, CheckList) -> ShootList -> (Board,Stack, CheckList)
aiShootAux (b, s@(coord,cell):st, cl) l 
  | isShip s = (checkCell b coord, fst $ filterDirection (removeChecked $ nub (cohesiveCells b s ++ st)) (updateCheckList (s:st) cl), (updateCheckList (s:st) cl))
  | otherwise = (checkCell b coord, st, cl)

-- Checks first cell in stack
aiShoot :: (Board,Stack, CheckList) -> StdGen -> ((Board, Stack, CheckList), StdGen)
aiShoot (b,s, cl) gen = (aiShootAux (b,removeChecked $ updateStack s newList, updateCheckList s cl) newList,newGen)
                     where (newList, newGen) = filterShootList b cl gen

-- ONLY CALL IF HIT
-- Takes the first element in Stack and adds it to CheckList
updateCheckList :: Stack -> CheckList -> CheckList
updateCheckList (x:xs) cl = x : cl

-- ONLY CALL IF HIT
-- Returns True if two elements are on the same Col
compareCol :: (CellCoord,Cell) -> (CellCoord,Cell) -> Bool
compareCol ((c1,r1),cell1) ((c2,r2),cell2)
  | c1 == c2 = True
  | otherwise = False

-- ONLY CALL IF HIT
-- Returns True if two elements are on the same Row
compareRow:: (CellCoord,Cell) -> (CellCoord,Cell) -> Bool
compareRow ((c1,r1),cell1) ((c2,r2),cell2)
  | r1 == r2 = True
  | otherwise = False

{-
filterDirection :: Stack -> CheckList -> Stack
filterDirection stack (y@((c,r),Ship Checked):ys)
  | isShip (head stack) && compareCol y (head stack) = filter (compareCol y) stack
  | isShip (head stack) && compareRow y (head stack) = filter (compareRow y) stack
  | otherwise = filterDirection (tail stack) (y:ys)
filterDirection stack _ = stack
-}

-- Removes elements from stack if they are the wrong direction of a ship and adds them to the CheckList
filterDirection :: Stack -> CheckList -> (Stack,CheckList)
filterDirection [] cl = ([],cl)
filterDirection stack [] = (stack,[])
filterDirection stack (y@((c,r),Ship NotChecked):ys)
  | isShip (head stack) && compareCol y (head stack) = (filter (compareCol y) stack,(y:ys) ++ (filter (compareRow y) stack))
  | isShip (head stack) && compareRow y (head stack) = (filter (compareRow y) stack,(y:ys) ++ (filter (compareCol y) stack))
  | otherwise = filterDirection (tail stack) (y:ys)
filterDirection stack (y:ys) = (stack,(y:ys))

-- Removes every element i CheckList from ShootList
removeShootList :: CheckList -> ShootList -> ShootList
removeShootList [] sl = sl
removeShootList (x:xs) sl = filter (/= x) (removeShootList xs sl)


-- Checks first cell in stack
--PRE: ShootList is not empty
aiShootAux' :: (Board,Stack, CheckList) -> (Board,Stack, CheckList)
aiShootAux' (b, s@(coord,cell):st, cl)
  | isShip s = (checkCell b coord, fst direction, [s] ++ snd direction)
  | otherwise = (checkCell b coord, st, [s] ++ cl)
    where direction = filterDirection (removeChecked $ nub (cohesiveCells b s ++ st)) (nub (cl ++ cohesiveCells b s))

-- Checks first cell in stack
aiShoot' :: ((Board,Stack, CheckList),StdGen) -> ((Board, Stack, CheckList), StdGen)
aiShoot' ((b,s,cl),gen) = (aiShootAux' (b,removeChecked $ updateStack s (removeShootList cl newList),cl), newGen)
    where (newList, newGen) = filterShootList b cl gen 

tester :: (Array (Int, Int) Cell, [a])
tester = (array ((0,0),(2,2)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Ship NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Ship NotChecked),((2,0),Ship NotChecked),((2,1),Ship NotChecked),((2,2),Empty NotChecked)],[])
tester2 :: (Array (Int, Int) Cell, [a])
tester2 = (array ((0,0),(2,2)) [((0,0),Ship NotChecked),((0,1),Ship NotChecked),((0,2),Ship NotChecked),((1,0),Ship NotChecked),((1,1),Ship NotChecked),((1,2),Ship NotChecked),((2,0),Ship NotChecked),((2,1),Ship NotChecked),((2,2),Ship NotChecked)],[])

tester3 = [((1,2),Ship NotChecked),((1,1),Ship NotChecked),((0,2),Empty NotChecked),((2,2),Empty NotChecked)]
tester4 = [((1,2),Ship NotChecked),((1,1),Empty NotChecked),((0,2),Ship NotChecked),((2,2),Ship NotChecked)]

tester5 :: Array (Int, Int) Cell
tester5 = array ((0,0),(3,3)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Empty NotChecked),((0,3),Empty NotChecked),((1,0),Ship NotChecked),((1,1),Ship NotChecked),((1,2),Ship NotChecked),((1,3),Ship NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Empty NotChecked),((2,3),Empty NotChecked),((3,0),Empty NotChecked),((3,1),Empty NotChecked),((3,2),Empty NotChecked),((3,3),Empty NotChecked)]
tester6 :: Array (Int, Int) Cell
tester6 = array ((0,0),(3,3)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((0,2),Ship NotChecked),((0,3),Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked),((1,2),Ship NotChecked),((1,3),Empty NotChecked),((2,0),Empty NotChecked),((2,1),Empty NotChecked),((2,2),Ship NotChecked),((2,3),Empty NotChecked),((3,0),Empty NotChecked),((3,1),Empty NotChecked),((3,2),Ship NotChecked),((3,3),Empty NotChecked)]