--
-- Sudoku.hs
--

--
-- This program reads a Sudoku problem from a file and
-- outputs the solution to stdout.
--

module Main where

import Control.Monad
import Data.Array.IO
import Data.Char
import System.Environment
import System.Exit
import System.IO
import qualified Data.Set as S


usage :: IO ()
usage = do
  hPutStrLn stderr $ "usage: sudoku filename"
  hPutStrLn stderr $ "  filename: string for input file"

type Sudoku = IOArray (Int, Int) Int


-- Read a file's contents into a Sudoku array.
readSudoku :: FilePath -> IO Sudoku
readSudoku f = do
  s <- readFile f
  let ls = lines s in
    if okLines ls
       then newListArray ((1, 1), (9, 9)) (map charToInt (concat ls))
       else error "readSudoku: invalid string input"
  where
    -- Check that the string input is a valid representation of a Sudoku board.
    okLines :: [String] -> Bool
    okLines ss =
      and [length ss == 9,
           all (\s -> length s == 9) ss,
           all okChar (concat ss)]

    okChar :: Char -> Bool
    okChar '.' = True
    okChar c | ord c >= ord '0' && ord c <= ord '9' = True
    okChar _ = False

    charToInt :: Char -> Int
    charToInt '.' = 0
    charToInt c   = ord c - ord '0'


-- Solve a Sudoku board.
-- Do this by iterating through the board, incrementing the unfilled numbers
-- by 1 until the right solution is found.
-- Return True if a solution is found, else false.
-- If a solution is found, the board contents will have mutated to the solution.
solveSudoku :: Sudoku -> IO Bool
solveSudoku s = iter s (1, 1)
  where
    -- Solve a Sudoku board starting from location (i, j).
    -- All "previous" locations are assumed to have been filled.
    -- If the board is solvable, return True; if not, return False.
    -- In the latter case, the board will not have changed.
    -- Skip all locations that are already filled.
    iter :: Sudoku -> (Int, Int) -> IO Bool
    iter _ (0, 0) = return True
    iter sud loc = do
      v <- readArray sud loc
      if v == 0 then (getOKValues sud loc) >>= (iter' sud loc)
        else iter sud $ nextLoc loc

    -- Try to solve the board using all possible currently-valid
    -- values at a particular location.
    -- If the board is unsolvable, reset the location to a zero
    -- (unmake the move) and return False.
    iter' :: Sudoku -> (Int, Int) -> [Int] -> IO Bool
    iter' _ _ [] = return False
    iter' sud loc (v:vs) = do
      writeArray sud loc v
      solvable <- iter sud $ nextLoc loc
      if solvable 
        then return True
        else writeArray sud loc 0 >> iter' sud loc vs

    -- Returns the next valid index assuming a 1-index 9 by 9 board
    nextLoc :: (Int, Int) -> (Int, Int)
    nextLoc (r, c) | r > 9 || c > 9 = 
      error "solveSudoku: index out of bounds error"
    nextLoc (9, 9) = (0, 0)
    nextLoc (r, c) | c == 9 = (r + 1, 1)
    nextLoc (r, c) = (r, c + 1)

    -- Get a list of indices that could be in a particular location on the 
    -- board (no conflicts in row, column, or box).
    getOKValues :: Sudoku -> (Int, Int) -> IO [Int]
    getOKValues sud (r, c) = do
      rs <- getRow sud r
      cs <- getCol sud c
      bs <- getBox sud (r, c)
      let setNotOK = S.fromList $ rs ++ cs ++ bs :: S.Set Int
      let setOK = S.difference (S.fromList [1..9]) setNotOK :: S.Set Int
      return $ S.toList setOK

    -- Return the ith row in a Sudoku board as a list of Ints.
    getRow :: Sudoku -> Int -> IO [Int]
    getRow sud r = 
      let idxs = range ((r, 1), (r, 9)) :: [(Int, Int)] in
        mapM (readArray sud) idxs

    -- Return the ith column in a Sudoku board as a list of Ints.
    getCol :: Sudoku -> Int -> IO [Int]
    getCol sud c = 
      let idxs = range ((1, c), (9, c)) :: [(Int, Int)] in
        mapM (readArray sud) idxs
  
    -- Return the box covering location (i, j) as a list of Ints.
    getBox :: Sudoku -> (Int, Int) -> IO [Int]
    getBox sud (r, c) = 
      let (rl, cl) = (r - (r-1) `mod` 3, c - (c-1) `mod` 3) in 
      let idxs = range ((rl, cl), (rl + 2, cl + 2)) :: [(Int, Int)] in
        mapM (readArray sud) idxs


-- Print a Sudoku board to stdout.
printSudoku :: Sudoku -> IO ()
printSudoku s = iter s 1 1
  where
    iter :: Sudoku -> Int -> Int -> IO ()
    iter s i j = 
      unless (i > 9)
        (do c <- readArray s (i, j)
            putChar $ intToChar c
            if j == 9 
               then putChar '\n' >> iter s (i + 1) 1
               else iter s i (j + 1))

    intToChar :: Int -> Char
    intToChar 0 = '.'
    intToChar n | n >= 1 && n <= 9 = intToDigit n
    intToChar m = error $ "printSudoku: invalid integer in array: " ++ show m


main :: IO ()
main = do
  args <- getArgs
  case args of
    [first] -> do 
      sudoku <- readSudoku first -- read board contents into array
      solved <- solveSudoku sudoku
      if solved
        then printSudoku sudoku >> exitSuccess
        else putStrLn "No solution exists." >> exitFailure
    _ -> usage >> exitFailure
