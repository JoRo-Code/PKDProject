import System.Random

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