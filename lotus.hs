--
-- Created by Sharynne Azhar - https://github.com/sharynneazhar/
-- Last update: 04/29/2016
--

import Data.List
import Data.List.Split
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

type Lotus = [Int]
type Solns = [Int]
type Indices = [Int]
type Index = Int


{--------------------------
-----  ACCESSORS ----------
--------------------------}

-- | A list of a list of indices for each left opening arc
leftArcs :: [Indices]
leftArcs = [[0,7,15,22,30,37,45],[1,8,16,23,31,38,46],[2,9,17,24,32,39,47],
            [3,10,18,25,33,40,48],[4,11,19,26,34,41,42],[5,12,20,27,28,35,43],
            [6,13,14,21,29,36,44]]

-- | A list of indices for each right opening arc
rightArcs :: [Indices]
rightArcs = [[0,13,20,26,33,39,46],[1,7,14,27,34,40,47],[2,8,15,21,28,41,48],
             [3,9,16,22,29,35,42],[4,10,17,23,30,36,43], [5,11,18,24,31,37,44],
             [6,12,19,25,32,38,45]]

-- | Finds and returns the list of indices containing that index value
-- Resource/Discussion at http://stackoverflow.com/questions/36878340/
getIndices :: (Ord a) => [[a]] -> a -> [a]
getIndices lst ind = fromMaybe [] (Map.lookup ind listOfIndices)
    where listOfIndices = Map.fromList mappedIndices
          mappedIndices = concatMap (\x -> zip x (repeat x)) lst

-- | Returns the index numbers of an arc/ring
getArcRings :: Index -> Indices
getArcRings ind = getRing ind ++ getIndices leftArcs ind ++ getIndices rightArcs ind
    where getRing n = [x..x + 6] where x = 7 * div n 7

-- | Returns the values from the lotus puzzle based on the indices
getValues :: Indices -> Lotus -> [Int]
getValues lts = map (lts !!)


{--------------------------
--------  SOLVER ----------
--------------------------}

-- | Creates a new lotus with the new value inserted at index given
returnBoard :: Int -> Index -> Lotus -> Lotus
returnBoard val ind lts = take ind lts ++ [val] ++ drop (ind + 1) lts

-- | Lists all the possible values that can be a solution to a particular ring/arc
possibleSolns :: Index -> Lotus -> Solns
possibleSolns ind lts
    | ind > 48 = []
    | lts !! ind == 0 = [1..7] \\ arcRingIndices ind
    | otherwise = [lts !! ind]
    where arcRingIndices n = getValues lts (getArcRings n)

-- | Solves the lotus puzzle using recursion (i.e. brute force method)
doSolve :: Index -> Solns -> Lotus -> Lotus
doSolve 48 [x] lts = returnBoard x 48 lts
doSolve 48 [] _ = []
doSolve 48 _ _ = []
doSolve _  [] _ = []
doSolve ind (x:xs) lts
    | null solvedNext = doSolve ind xs lts
    | otherwise = solvedNext
    where recurseNext n s = doSolve (n + 1) (possibleSolns (n + 1) s) s
          solvedNext = recurseNext ind (returnBoard x ind lts)

-- | Higher order solve method to tie the rest together
lotusSolver :: [Int] -> [Int]
lotusSolver lts = doSolve 0 (possibleSolns 0 lts) lts


{--------------------------
-----  HELPERS/TESTS ------
--------------------------}

-- | Prints a readable lotus in matrix form to console
-- Resource from http://stackoverflow.com/questions/12791400
printLotus :: (Show e) => [e] -> String
printLotus lts = if null lts then "\nNo solution\n"
                 else "\n" ++ unlines (map show (chunksOf 7 lts))

-- | Checks if the arc/ring contains the numbers 1 to 7
-- Resource from stackoverflow.com/questions/31036474/
checkValues :: Indices -> Bool
checkValues ind = all (`elem` [1..7]) ind && allDifferent ind
    where allDifferent = comparePairwise.sort
          comparePairwise n = and (zipWith (/=) n (drop 1 n))

-- | Checks all arcs and rings containing the given index
checkAll :: Lotus -> Index -> Bool
checkAll lts ind = all func [getIndices leftArcs ind, getIndices rightArcs ind, getRing ind]
                   where getRing n = [x..x + 6] where x = 7 * div n 7
                         func n = checkValues (getValues lts n)

runTest :: String -> Lotus -> String
runTest name lts = do
    let solvedLotus = lotusSolver lts
    if all (checkAll solvedLotus) [0..48] then show name ++ " passed!"
    else "### " ++ show name ++ " failed! ###"

runFailedTest :: String -> Lotus -> String
runFailedTest name lts
    | null solvedLotus = printSuccess
    | not (all (checkAll solvedLotus) [0..48]) = printSuccess
    | otherwise = "### " ++ show name ++ " failed! ###"
    where solvedLotus = lotusSolver lts
          printSuccess = show name ++ " passed!"


{--------------------------
---------- MAIN -----------
--------------------------}

main :: IO()
main = do
    putStrLn "\nTestA\n===================="
    putStrLn $ "Before:" ++ printLotus testA
    putStrLn $ "After:" ++ printLotus (lotusSolver testA)
    putStrLn $ "Check: " ++ runTest "testA" testA
    putStrLn "\nSuccess Cases\n===================="
    putStrLn $ runTest "testB" testB
    putStrLn $ runTest "testC" testC
    putStrLn $ runTest "testD" testD
    putStrLn $ runTest "testE" testE
    putStrLn "\n\nFailed Cases\n===================="
    putStrLn $ runFailedTest "testF" testF
    putStrLn $ runFailedTest "testG" testG


{--------------------------
--- EXAMPLES PUZZLES ------
--------------------------}

-- Success Tests
testA :: Lotus
testA = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,0,0,0,3,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0]

testB :: Lotus
testB = [4,1,2,3,6,0,0,0,0,0,0,1,0,0,0,1,7,4,0,0,2,0,0,0,0,1,0,
         5,3,0,0,4,0,0,0,0,7,0,0,0,0,0,0,1,2,0,0,0,0]

testC :: Lotus
testC = [0,1,0,7,6,0,0,4,0,0,1,0,0,0,0,0,6,0,0,5,0,0,0,0,0,0,0,
         5,0,0,0,0,0,2,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0]

testD :: Lotus
testD = [4,0,5,3,0,1,7,1,7,0,0,0,0,0,0,0,6,0,0,5,2,1,2,3,0,0,0,
         5,6,0,7,4,0,1,3,0,0,0,0,0,0,0,1,4,0,6,0,7,0]

testE :: Lotus
testE = [5,0,0,0,1,6,0,0,0,0,3,0,0,0,7,0,6,2,1,0,0,0,1,7,0,0,6,
         0,0,5,0,3,6,7,2,0,0,2,1,0,0,4,0,0,4,0,0,1,0]


-- Fail Tests
testF :: Lotus
testF = [4,4,7,2,1,6,3,6,5,4,3,7,2,1,7,3,6,2,1,5,4,2,1,7,5,4,6,3,
         1,5,4,3,6,7,2,7,6,2,1,3,5,4,3,5,4,7,2,1,6]

testG :: Lotus
testG = [5,0,0,0,1,6,0,0,5,5,3,0,0,0,7,0,6,2,1,0,0,0,1,7,0,0,6,
         0,0,5,0,3,6,7,2,0,0,2,1,0,0,4,0,0,4,0,0,1,0]
