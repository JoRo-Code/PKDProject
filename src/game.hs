module Game where

import Data.Array
import System.Random

data SquareState  = Checked           | NotChecked                 deriving (Show, Eq)
data Cell         = Empty SquareState | Ship SquareState           deriving (Show, Eq)
data Player       = User              | AI                         deriving (Show, Eq)
data GameStage    = Placing Player    | Shooting Player            deriving (Show, Eq)
data Direction    = Horizontal        | Vertical                   deriving (Show, Eq)  

type Row         = Int
type Col         = Int
type CellCoord   = (Col, Row)
type ScreenCoord = (Float, Float)
type Board       = Array (Col, Row) Cell
type BoardSize   = Int
type ShipSize    = Int

-- AI-types
type ShootList = [(CellCoord,Cell)]
type Stack = [(CellCoord,Cell)]
type AIHits = Board


{- cellCoord: starting coord of ship,
  Direction: continuing right or down,
  ShipSize: length of the ship
-}

type Ship = (CellCoord, Direction, ShipSize)
type Ships = [Ship]


type Radius = Float
type Pos = (Float, Float)
type Derivative = Float

type Radar = (Radius, Radius, Radius, Radius, Radius)


carrier :: Ship
carrier = ((0,0), Horizontal,5)
battleShip :: Ship
battleShip = ((0,3), Vertical,4)
cruiser :: Ship
cruiser = ((0,0), Horizontal,3)
submarine :: Ship
submarine = ((0,2), Vertical,3)
destroyer :: Ship
destroyer = ((0,0), Horizontal,2)





screenWidth :: Float
screenWidth = 1500
screenHeight :: Float
screenHeight = (screenWidth - screenDivider) / 2

-- filling between both boards
screenDivider :: Float
screenDivider = 300

--------------------- Solving cellWidth and cellHeight problem ---------------------


cellWidth :: Float
cellWidth  = (screenWidth - screenDivider) * 0.5 / fromIntegral n
cellHeight :: Float
cellHeight = screenHeight / fromIntegral n
boardWidth :: Float
boardWidth = (screenWidth - screenDivider) / 2
boardHeight :: Float
boardHeight = screenHeight




startRadius = 0
startDerivative = screenWidth/2

data Game = Game { gameBoardUser :: Board , 
                   gameBoardAI   :: Board,
                   gameBoardsAI  :: [Board],
                   hitsAI        :: AIHits,
                   gameStage     :: GameStage,
                   shipsUser     :: Ships,
                   stackAI       :: Stack,
                   winner        :: Maybe Player,
                   gen           :: StdGen,
                   currentRound  :: Int,
                   stats         :: ((Player, Int), (Player, Int)),
                   -- Animation
                   -- radius, position of centre of wave, end radius of wave, derivative, showBool
                   shootAnimation :: (Radius, Pos, Radius, Derivative, Bool),
                   radarAnimation :: Radar
                 } deriving (Show, Eq)

n :: BoardSize
n = 10

-- Create a new board, a 2d array, where all cells are empty notchecked initially.                                                         
initBoard :: Board        
initBoard = array boardIndex $ zip (range boardIndex) (repeat $ Empty NotChecked)
            where boardIndex = ((0, 0), (n - 1, n - 1)) 

initShips :: Ships
initShips = [carrier, battleShip, cruiser, submarine, destroyer]

initGame :: Game
initGame = Game { gameBoardUser = initBoard,
                  gameBoardAI   = initBoard, 
                  hitsAI        = initBoard,
                  gameStage     = Placing User,
                  shipsUser     = initShips,
                  stackAI       = [],
                  winner        = Nothing,
                  gen           = mkStdGen 100,
                  currentRound  = 1,
                  stats         = ((User, 0), (AI, 0)),

                  -- Animation
                  shootAnimation = (startRadius, (screenWidth/2,screenHeight/2), cellWidth/2, startDerivative, False),
                  radarAnimation = (1, 2, 3, 4, 5)
                }
