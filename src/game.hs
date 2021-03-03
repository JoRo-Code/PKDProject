module Game where

import Data.Array
import System.Random

{- The state of a square. 
    Checked means the square has been shot at.
    NotChecked means the square has not been shot at.
-}
data SquareState = Checked | NotChecked deriving (Show, Eq)

{- Information regarding a cell. 
    - Empty means the cell does not contain a ship.
    - Ship means the cell contains a ship.
    - SquareState gives the state of the cell.
-}
data Cell = Empty SquareState | Ship SquareState deriving (Show, Eq)

{- Represents current player.
    - User means it is the user currently playing.
    - AI means it is the AI currently playing.
-}
data Player = User | AI deriving (Show, Eq)

{- The current stage of the game and the current player.
     - Placing means the current player can place ships.
     - Shooting means the current player can shoot.
     - Player represents the current player.
-}
data GameStage = Placing Player | Shooting Player deriving (Show, Eq)

{- Represents the direction of a ship.
-}
data Direction = Horizontal | Vertical deriving (Show, Eq)  

{- The number of a row on the board.
-}
type Row         = Int

{- The number of a column on the board.
-}
type Col         = Int

{- Coordinates for a cell consisting of column and row.
-}
type CellCoord   = (Col, Row)

{- ... description of what the data type represents ... 
     ... description of how the datatype represents data ...
     INVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
type ScreenCoord = (Float, Float)

{- An Array with coordinates for a cell consisting of column and row and its corresponding cell.
-}
type Board       = Array (Col, Row) Cell

{- The size of a board where the BoardSize is the amount of Columns and Rows.
-}
type BoardSize   = Int

{- The length of a Ship represented by an Int.
-}
type ShipSize    = Int

{- ... description of what the data type represents ... 
     ... description of how the datatype represents data ...
     INVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
type Round       = Int

{- ... description of what the data type represents ... 
     ... description of how the datatype represents data ...
     INVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
type Score       = Int

{- ... description of what the data type represents ... 
     ... description of how the datatype represents data ...
     INVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
type Stats       = ((Player, Score), (Player, Score))

{- ... description of what the data type represents ... 
     ... description of how the datatype represents data ...
     INVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
type HitShip     = Bool

{- A list of the Coordinates and Cells AI can shoot.
-}
type ShootList = [(CellCoord,Cell)]


{- A list of the Coordinates and Cells AI is currently focusing its shots.
-}
type Stack = [(CellCoord,Cell)]

{- ... description of what the data type represents ... 
     ... description of how the datatype represents data ...
     INVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
type Ship = (CellCoord, Direction, ShipSize)

{- ... description of what the data type represents ... 
     ... description of how the datatype represents data ...
     INVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
type Ships = [Ship]

{- ... description of what the data type represents ... 
     ... description of how the datatype represents data ...
     INVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
type Radius = Float

{- ... description of what the data type represents ... 
     ... description of how the datatype represents data ...
     INVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
type Pos = (Float, Float)

{- ... description of what the data type represents ... 
     ... description of how the datatype represents data ...
     INVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
type Derivative = Float

{- ... description of what the data type represents ... 
     ... description of how the datatype represents data ...
     INVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
type Angle = Float

{- ... description of what the data type represents ... 
     ... description of how the datatype represents data ...
     INVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
type Radar = ([Radius], Angle)

{- ... description of what the data type represents ... 
     ... description of how the datatype represents data ...
     INVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
data Game = Game { gameBoardUser  :: Board , 
                   gameBoardAI    :: Board,
                   gameStage      :: GameStage,
                   shipsUser      :: Ships,
                   stackAI        :: Stack,
                   winner         :: Maybe Player,
                   gen            :: StdGen,
                   currentRound   :: Round,
                   stats          :: Stats,
                   shootAnimation :: (HitShip, Radius, Pos, Radius, Derivative, Bool),
                   radarAnimation :: Radar,
                   radarAngle     :: Angle
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
                  gameStage     = Placing User,
                  shipsUser     = initShips,
                  stackAI       = [],
                  winner        = Nothing,
                  gen           = mkStdGen 100,
                  currentRound  = 1,
                  stats         = ((User, 0), (AI, 0)),
                  shootAnimation = (False, startRadius, (screenWidth/2,screenHeight/2), cellWidth/2, startDerivative, False),
                  radarAnimation = radarInitial,
                  radarAngle          = 0.0
                }
                where radarInitial = ([maxRadius - i * radiusOffet | i <- [0..4]], 0)
                      radiusOffet  = (screenHeight / 2) / 5
                      maxRadius = screenHeight / 2
