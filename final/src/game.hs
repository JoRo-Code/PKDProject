module Game where
import Data.Array
import System.Random

{- The state of a square. 
    - Checked means the square has been shot at.
    - NotChecked means the square has not been shot at.
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
     - Horizontal means the ship has a horizontal direction.
     - Vertical means the ship has a vertical direction.
-}
data Direction = Horizontal | Vertical deriving (Show, Eq)  

{- Represents the current state of the game. 
     - gameBoardUser represents the user's board.
     - gameBoardAI represents the AI's board.
     - gameStage represents the current stage of the game.
     - shipsUser represents the ships given to the user.
     - stackAI represents the current stack for the AI.
     - winner represents the winner of the current round. If no one has won yet, it is Nothing.
     - gen represents the latest random seed.
     - currentRound represents the current round.
     - stats represents the current user and AI stats.
     - shootAnimation holds all information for the shooting animations.
     - radarAnimation holds all information for the radar background animation.
     INVARIANT: 
     - Ships cannot be placed right next to each other. 
     - Ships must not be overlapped.
     - Ships must be located within the boundaries of the board.
     - Shots must be withing the boundaries of the board.
     - Shots cannot be fired on already shot cells.
     - Given ships has to be able to fit within the board according the the placement rules.
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
                   shootAnimation :: (HitShip, Radius, ScreenCoord, Radius, Derivative, Bool),
                   radarAnimation :: Radar
                 } deriving (Show, Eq)

-- The number of a row on the board.
type Row = Int

-- The number of a column on the board.
type Col = Int

-- Coordinates for a cell consisting of column and row.
type CellCoord = (Col, Row)

-- Coordinates for position on gloss window.
type ScreenCoord = (Float, Float)

-- Represents the boundaries of a board on a gloss window.
type BoardPos = (ScreenCoord, ScreenCoord)

-- An Array with coordinates for a cell consisting of column and row and its corresponding cell.
type Board = Array (Col, Row) Cell

-- The size of a board where the BoardSize is the amount of Columns and Rows.
type BoardSize = Int

-- The length of a Ship represented by an Int.
type ShipSize = Int

-- Represents the current round.
type Round = Int

-- Represents the current score.
type Score = Int

-- Represents two players and their corresponding score.
type Stats = ((Player, Score), (Player, Score))

-- The status of whether a Ship is being hit or not.
type HitShip = Bool

-- A list of the Coordinates and Cells AI can shoot.
type ShootList = [(CellCoord,Cell)]

-- A list of the Coordinates and Cells where the AI is currently focusing its shots.
type Stack = [(CellCoord,Cell)]

-- Represents a ship with its starting coordinates, direction and length.
type Ship = (CellCoord, Direction, ShipSize)

-- A list of ships with their starting coordinates, directions and length.
type Ships = [Ship]

-- Represents the radius of a circle.
type Radius = Float

-- Represents the speed of an increasing radius in an animation.
type Derivative = Float

-- Represents the angle of the rotating arc in the radar.
type Angle = Float

-- Represents the radiuses of all circles in the radar and the angle of the arc. 
type Radar = ([Radius], Angle)

screenWidth :: Float
screenWidth = 1440 -- the width of the gloss window

screenHeight :: Float
screenHeight = (screenWidth - screenDivider) / 2 -- the height of the gloss window

screenDivider :: Float
screenDivider = 300 -- filling between both boards

cellWidth :: Float
cellWidth  = (screenWidth - screenDivider) * 0.5 / fromIntegral n -- the width of a cell

cellHeight :: Float
cellHeight = screenHeight / fromIntegral n -- the height of a cell

boardWidth :: Float
boardWidth = (screenWidth - screenDivider) / 2 -- the width of a board

boardHeight :: Float
boardHeight = screenHeight -- the height of a board

boardAIPos :: BoardPos
boardAIPos = ((screenWidth * 0.5 + screenDivider * 0.5 ,0), (screenWidth, screenHeight)) -- the boundaries of the AI board

boardUserPos :: BoardPos
boardUserPos = ((0,0), (screenWidth * 0.5 - screenDivider * 0.5, screenHeight)) -- the boundaries of the user board

startRadius :: Radius
startRadius = 0 -- the starting radius for explosion animation

startDerivative :: Derivative
startDerivative = screenWidth/2 -- the starting derivative for explosion animation

n :: BoardSize
n = 10 -- gives a board of size n * n

initBoard :: Board        
initBoard = array boardIndex $ zip (range boardIndex) (repeat $ Empty NotChecked) -- Creates initial board of n-size with empty cells 
            where boardIndex = ((0, 0), (n - 1, n - 1)) 

-- Given ships:
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

initGame :: Game -- Creates the initial game
initGame = Game { gameBoardUser = initBoard, 
                  gameStage     = Placing User,
                  shipsUser     = initShips,
                  stackAI       = [],
                  winner        = Nothing,
                  gen           = mkStdGen 100,
                  currentRound  = 1,
                  stats         = ((User, 0), (AI, 0)),
                  shootAnimation = (False, startRadius, (screenWidth/2,screenHeight/2), cellWidth/2, startDerivative, False),
                  radarAnimation = radarInitial
                }
                where radarInitial = ([maxRadius - i * radiusOffet | i <- [0..4]], 0)
                      radiusOffet  = (screenHeight / 2) / 5
                      maxRadius = screenHeight / 2
