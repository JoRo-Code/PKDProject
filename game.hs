import Data.Array


data Cell = Empty State | Ship State deriving (Show)

data State = Checked | NotChecked deriving (Show)

type Board = Array (Int, Int) Cell

initBoard n = array boardIndex $ zip (range boardIndex) (repeat (Empty NotChecked))
            where boardIndex = ((0, 0), (n - 1, n - 1))