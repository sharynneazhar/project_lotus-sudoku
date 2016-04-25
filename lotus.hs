--
-- Copyright (c) 2016 Sharynne Azhar - https://github.com/sharynneazhar/
-- Last update: 04/25/2016
--

import Data.List
import Data.List.Split

type Lotus = [Int]
type Indices = [Int]
type Index = Int

data ArcType
    -- | Arcs that curve to the left
    = OpenLeft Index
    -- | Arcs that curve to the right
    | OpenRight Index

puzzle :: Lotus
puzzle = [5, 0, 7, 0, 1, 6, 0, -- ring 1
          0, 0, 0, 3, 0, 0, 0, -- ring 2
          7, 0, 6, 2, 1, 0, 0, -- ring 3
          0, 1, 7, 0, 0, 6, 0, -- ring 4
          0, 5, 0, 3, 6, 7, 2, -- ring 5
          0, 0, 2, 1, 0, 0, 4, -- ring 6
          0, 0, 4, 0, 0, 1, 0] -- ring 7

solved :: Lotus
solved = [5, 4, 7, 2, 1, 6, 3,
          6, 5, 4, 3, 7, 2, 1,
          7, 3, 6, 2, 1, 5, 4,
          2, 1, 7, 5, 4, 6, 3,
          1, 5, 4, 3, 6, 7, 2,
          7, 6, 2, 1, 3, 5, 4,
          3, 5, 4, 7, 2, 1, 6]


{--------------------------
-----  ACCESSORS ----------
--------------------------}

-- | Gets the index numbers of a ring
getRing :: Index -> Indices
getRing index = [x + 7 * index | x <- [0..6]]

-- | Gets the index numbers of an arc
getArc :: ArcType -> Indices
getArc (OpenLeft n)
    | n == 0 = [0,7,15,22,30,37,45]
    | n == 1 = [1,8,16,23,31,38,46]
    | n == 2 = [2,9,17,24,32,39,47]
    | n == 3 = [3,10,18,25,33,40,48]
    | n == 4 = [4,11,19,26,34,41,42]
    | n == 5 = [5,12,20,27,28,35,43]
    | n == 6 = [6,13,14,21,29,36,44]
    | otherwise = []
getArc (OpenRight n)
    | n == 0 = [0,13,20,26,33,39,46]
    | n == 1 = [1,7,14,27,34,40,47]
    | n == 2 = [2,8,15,21,28,41,48]
    | n == 3 = [3,9,16,22,29,35,42]
    | n == 4 = [4,10,17,23,30,36,43]
    | n == 5 = [5,11,18,24,31,37,44]
    | n == 6 = [6,12,19,25,32,38,45]
    | otherwise = []

-- | Gets the values from the lotus puzzle based on the indices
getValues :: Indices -> Lotus -> [Int]
getValues indices lotus = map (lotus !!) indices


{--------------------------
--------  CHECKS ----------
--------------------------}

-- | Compares indices in the list in pairs
-- Resource from stackoverflow.com/questions/31036474/
comparePairwise :: Eq a => [a] -> Bool
comparePairwise indices = and (zipWith (/=) indices (drop 1 indices))

-- | Checks if the values in indices are uniquely 1 to 7 using comparePairwise
-- Resource from stackoverflow.com/questions/31036474/
allDifferent :: (Ord a, Eq a) => [a] -> Bool
allDifferent = comparePairwise.sort

-- | Checks if the arc/ring contains the numbers 1 to 7
checkValues :: Indices -> Bool
checkValues indices = all (`elem` [1..7]) indices && allDifferent indices

-- | Checks all arcs and rings containing the given index
checkAll :: Lotus -> Index -> Bool
checkAll lotus index = checkValues (getValues (getArc (OpenLeft index)) lotus) &&
                       checkValues (getValues (getArc (OpenRight index)) lotus) &&
                       checkValues (getValues (getRing index) lotus)

{--------------------------
--------  SOLVER ----------
--------------------------}

-- | Finds the next black in the puzzle starting from index
findBlank :: Index -> Lotus -> Index
findBlank index lotus
    | index == 48 = 48
    | lotus !! (index + 1) == 0 = index + 1
    | otherwise = findBlank (index + 1) lotus

-- | Creates a new lotus with the new value inserted at index given
insertValue :: Int -> Index -> Lotus -> Lotus
insertValue value index lotus = take index lotus ++ [value] ++ drop (index + 1) lotus

-- | Lists all the possible values that can be a solution to a particular ring/arc
-- Possibilities are any values [1,7] that are not in the ring/arc
possibleValues :: Indices -> Lotus -> [Int]
possibleValues indices lotus = [1..7] \\ (ring ++ leftArc ++ rightArc)
    where ring = undefined
          leftArc = undefined
          rightArc = undefined


lotusSolver :: [Int] -> [Int]
lotusSolver lotus = lotus

{--------------------------
-------  HELPERS ----------
--------------------------}

-- | Print a readable lotus in matrix form to console
printLotus :: (Show e) => [e] -> String
printLotus lotus = unlines (map show (chunksOf 7 lotus))


{--------------------------
---------- MAIN -----------
--------------------------}

main :: IO()
main =
    -- print (allDifferent (getValues (getRing 0) solved))    -- expect True
    -- print (map (checkAll puzzle) [0..6])                   -- expect all false
    -- print (map (checkAll solved) [0..6])                   -- expect all true
    -- print (getValues (getRing 0) puzzle)                   -- expect [5,0,0,0,1,6,0]
    -- print (findBlank 0 puzzle)                             -- expect 3
    -- putStrLn $ printLotus (insertValue 4 1 puzzle)         -- expect 4 at second position
    putStrLn $ printLotus (lotusSolver puzzle)
