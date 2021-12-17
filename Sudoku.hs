module Sudoku where

import Data.Char (digitToInt, intToDigit)
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe)
import Data.List (transpose, group, sort, elemIndex, nub, findIndex)
import Data.List.Split (chunksOf)

-------------------------------------------------------------------------

{-| A Sudoku puzzle is a list of lists, where each value is a Maybe Int. That is,
each value is either `Nothing' or `Just n', for some Int value `n'. |-}
newtype Puzzle = Puzzle [[Maybe Int]]
 deriving (Show, Eq)

{-| A Block is a list of 9 Maybe Int values. Each Block represents a row, a column,
or a square. |-}
type Block = [Maybe Int]

{-| A Pos is a zero-based (row, column) position within the puzzle. |-}
newtype Pos = Pos (Int, Int) deriving (Show, Eq)

{-| A getter for the rows in a Sudoku puzzle. |-}
rows :: Puzzle -> [[Maybe Int]]
rows (Puzzle rs) = rs

example :: Puzzle
example =
  Puzzle
    [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
    , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
    , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
    , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
    , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
    , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
    , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
    , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
    , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
    ]

{-| Ex 1.1

    A sudoku with just blanks. |-}
allBlankPuzzle :: Puzzle
--replicating Nothing 9 times for the 9 rows of the sudoku
allBlankPuzzle = Puzzle $ replicate 9 (replicate 9 Nothing)

{-| Ex 1.2

    Checks if sud is really a valid representation of a sudoku puzzle. |-}
isPuzzle :: Puzzle -> Bool
--number of rows and values are checked to ensure it fits the pattern of a sudoku puzzle
isPuzzle puzzle = length (rows puzzle) == 9 && 
    and [length row ==9 | row <- rows puzzle] && 
    and [forevery row | row <- rows puzzle] where
        forevery row = and [forevery' cell | cell <- row] where
            forevery' Nothing = True
            forevery' (Just n) = n <= 9 && n>=0 

{-| Ex 1.3

    Checks if the puzzle is already solved, i.e. there are no blanks. |-}
isSolved :: Puzzle -> Bool
--returns true if Nothing is not an element of row and false if it is
isSolved puzzle = and [Nothing `notElem` row | row <- rows puzzle]

{-| Ex 2.1

    `printPuzzle s' prints a representation of `s'. |-}

--Helps to build the rows containing the values
buildPuzzle puzzle = concat [buildRow row ++ "\n" | row <- rows puzzle] where
    buildRow r = [buildCell cell | cell <- r] where
        buildCell Nothing = '.'
        buildCell (Just n) = intToDigit n

printPuzzle :: Puzzle -> IO ()
printPuzzle puzzle = putStrLn (buildPuzzle puzzle)

{-| Ex 2.2

    `readPuzzle f' reads from the FilePath `f', and either delivers it, or stops
    if `f' did not contain a puzzle. |-}

readPuzzle :: FilePath -> IO Puzzle

--reads file and keeps its result which is converted to a block where values such as '.' are transformed to Nothing and then finally returned as puzzle

readPuzzle f = ((\ x -> if isPuzzle x then x else error "Not a valid Sudoku puzzle") . Puzzle . map toBlock . lines) <$> readFile f


toBlock :: String -> Block
toBlock [] = [] 
toBlock (x:xs) = case x of
    '.' -> Nothing : toBlock xs
    '\n' -> toBlock xs 
    _ -> Just (digitToInt x) : toBlock xs 
  

{-| Ex 3.1

    Check that a block contains no duplicate values. |-}
isValidBlock :: Block -> Bool
--check if current block is equal to block after duplicates eliminated and filter to make sure there aren't multiple nothing values
isValidBlock block = block' == nub block'
                     where block' = filter isJust block

{-| Ex 3.2

    Collect all blocks on a board - the rows, the columns and the squares. |-}
blocks :: Puzzle -> [Block]
blocks puzzle = r ++ transpose r ++ (map concat . concatMap transpose . chunksOf 3 . map(chunksOf 3)) r
    where r = rows puzzle

{-| Ex 3.3

    Check that all blocks in a puzzle are legal. |-}
isValidPuzzle :: Puzzle -> Bool
isValidPuzzle puzzle = and [isValidBlock block | block <- blocks puzzle]

{-| Ex 4.1

    Given a Puzzle that has not yet been solved, returns a position in
    the Puzzle that is still blank. If there are more than one blank
    position, you may decide yourself which one to return. |-}
blank :: Puzzle -> Pos
--finds the index for the first blank value from xs which is full of blank columns, then a (Int, Int) tuple is created  with the value of the first blank row and the value of y
blank puzzle = let xs = map (elemIndex Nothing) (rows puzzle)
                   y = findIndex isJust xs
               in case y of
                   Just n -> Pos (n, fromJust ((!!)  xs n))
{-| Ex 4.2

    Given a list, and a tuple containing an index in the list and a
    new value, updates the given list with the new value at the given
    index. |-}
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] _ = [] 
(!!=) arr (index, value) 
  --values are concatenated before index and new values are after
  | (index >= length arr) || (index < 0) = arr
  | otherwise = take index arr ++ [value] ++ drop (index+1) arr

{-| Ex 4.3

    `update s p v' returns a puzzle which is a copy of `s' except that
    the position `p' is updated with the value `v'. |-}
update :: Puzzle -> Pos -> Maybe Int -> Puzzle
update s (Pos(y,x)) v = Puzzle $ 
                        take y (rows s) ++ 
                        [row !!= (x,v)] ++ 
                        drop (y+1) (rows s) where
                          row = rows s !! y

{-| Ex 5.1

    Solve the puzzle. |-}
solve :: Puzzle -> Maybe Puzzle
solve s | not (isValidPuzzle s) = Nothing --violation in sudokuso no result
        | isSolved s            = Just s --sudoku already solved so just return
        | otherwise             = pickASolution possibleSolutions 

--blank values filled with all numbers 1-9 trying to find the scenario which the integers are a specific solution by recursively searching through the puzzle
  where 
    
    nineUpdatedSuds    = [update s (blank s) (Just v) | v <- [1..9]] :: [Puzzle]
    possibleSolutions  = [solve s' | s' <- nineUpdatedSuds]

pickASolution :: [Maybe Puzzle] -> Maybe Puzzle
pickASolution [] = Nothing 
pickASolution (x:xs) = if isJust x then x else pickASolution xs 

{-| Ex 5.2

    Read a puzzle and solve it. |-}
readAndSolve :: FilePath -> IO (Maybe Puzzle)
readAndSolve f = do
                   puzzle <- readPuzzle f 
                   pure $ solve puzzle 

{-| Ex 5.3

    Checks if s1 is a solution of s2. |-}
isSolutionOf :: Puzzle -> Puzzle -> Bool
--If value is Nothing then ignore otherwise check if x and y are equal then resulting in a list of tuples with the elements of the two sudoku puzzles
isSolutionOf s1 s2 = isValidPuzzle s1 
                     && isSolved s1 
                     && all (\(x,y) -> isNothing y || (x==y))
                            (zip ((concat . rows) s1) ((concat . rows) s2))

