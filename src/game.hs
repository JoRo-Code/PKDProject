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
type Round       = Int
type Score       = Int
type Stats       = ((Player, Score), (Player, Score))
type HitShip     = Bool
-- AI-types
type ShootList = [(CellCoord,Cell)]
type Stack = [(CellCoord,Cell)]

{- cellCoord: starting coord of ship,
  Direction: continuing right or down,
  ShipSize: length of the ship
-}

type Ship = (CellCoord, Direction, ShipSize)
type Ships = [Ship]


type Radius = Float
type Pos = (Float, Float)
type Derivative = Float

type Angle = Float
type Radar = ([Radius], Angle)

data Game = Game { gameBoardUser :: Board , 
                   gameBoardAI   :: Board,
                   gameStage     :: GameStage,
                   shipsUser     :: Ships,
                   stackAI       :: Stack,
                   winner        :: Maybe Player,
                   gen           :: StdGen,
                   currentRound  :: Round,
                   stats         :: Stats,
                   -- Animation
                   -- radius, position of centre of wave, end radius of wave, derivative, showBool
                   shootAnimation :: (HitShip, Radius, Pos, Radius, Derivative, Bool),
                   radarAnimation :: Radar,
                   radarAngle          :: Angle
                 } deriving (Show, Eq)


type BoardPos = (ScreenCoord, ScreenCoord)


screenWidth :: Float
screenWidth = 1440
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

boardAIPos :: BoardPos
boardAIPos = ((screenWidth * 0.5 + screenDivider * 0.5 ,0), (screenWidth, screenHeight))
boardUserPos :: BoardPos
boardUserPos = ((0,0), (screenWidth * 0.5 - screenDivider * 0.5, screenHeight))




startRadius = 0
startDerivative = screenWidth/2



n :: BoardSize
n = 10

-- Create a new board, a 2d array, where all cells are empty notchecked initially.   

{- initBoard
    Creates initial board of n-size with empty cells 
    PRE: 
    RETURNS: array of empty cells of size n
    EXAMPLES: initBoard == array ((0,0),(1,1)) [((0,0),Empty NotChecked),((0,1),Empty NotChecked),((1,0),Empty NotChecked),((1,1),Empty NotChecked)]
                  where n = 2
-} 
initBoard :: Board        
initBoard = array boardIndex $ zip (range boardIndex) (repeat $ Empty NotChecked)
            where boardIndex = ((0, 0), (n - 1, n - 1)) 


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

initShips :: Ships
initShips = [carrier, battleShip, cruiser, submarine, destroyer]


{- initGame 
  Creates the initial game

-}
initGame :: Game
initGame = Game { gameBoardUser = initBoard,
                  gameBoardAI   = initBoard, 
                  gameStage     = Placing User,
                  shipsUser     = initShips,
                  stackAI       = [],
                  winner        = Nothing,
                  gen           = mkStdGen 100,
                  currentRound  = 1,
                  stats         = ((User, 0), (AI, 0)),

                  -- Animation
                  shootAnimation = (False, startRadius, (screenWidth/2,screenHeight/2), cellWidth/2, startDerivative, False),
                  radarAnimation = radarInitial,
                  radarAngle          = 0.0
                }
                where radarInitial = ([maxRadius - i * radiusOffet | i <- [0..4]], 0)
                      radiusOffet  = (screenHeight / 2) / 5
                      maxRadius = screenHeight / 2
