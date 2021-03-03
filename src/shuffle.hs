module Shuffle where
import System.Random

{- shuffle list seed
    shuffles a list with a seed
    RETURNS: (shuffled list with seed, newGen of seed)
    EXAMPLES: 
                shuffle [1..5] (mkStdGen 10)                    == ([2,3,1,5,4],                   StdGen {unStdGen = SMGen 10920071015943500239 614480483733483467})
                shuffle [] (mkStdGen 10)                        == ([],                            mkStdGen 10)
                shuffle "hello" (mkStdGen 10)                   == ("elhol",                       StdGen {unStdGen = SMGen 10920071015943500239 614480483733483467})
-}

shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle [] gen = ([], gen)
shuffle list gen = shuffleAux list [] gen


{- shuffleAux list newList seed
    adds shuffled list to newList
    RETURNS: (shuffled list with seed concatenated with newList, newGen of seed) 
    EXAMPLES:   
                shuffleAux [1..5]  []        (mkStdGen 10)      == ([2,3,1,5,4],                    StdGen {unStdGen = SMGen 10920071015943500239 614480483733483467})
                shuffleAux [1..5]  [6..9]    (mkStdGen 10)      == ([2,3,1,5,4,6,7,8,9],            StdGen {unStdGen = SMGen 10920071015943500239 614480483733483467})
                shuffleAux []      [6..9]    (mkStdGen 10)      == ([6..9],                         mkStdGen 10)
                shuffleAux "hello" " world"  (mkStdGen 10)      == ("elhol world",                  StdGen {unStdGen = SMGen 10920071015943500239 614480483733483467})
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
    removes element of list at index
    PRE: -
    RETURNS: a list with the element at position index removed
    EXAMPLES:   
                removeIndex [1,2,3] 1       == [1,3]
                removeIndex [1,2,3] (-1)    == [1,2,3]
                removeIndex []      1       == []
                removeIndex "hello" 3       == "helo"
-}

removeIndex :: [b] -> Int -> [b]
removeIndex list index = map snd $ filter (\(i, element) -> i /= index) (zip [0..length list - 1] list)