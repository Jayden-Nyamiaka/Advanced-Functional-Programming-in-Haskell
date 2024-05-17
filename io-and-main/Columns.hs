-- Columns.hs
module Main where

import Prelude
import System.Environment
import System.Exit
import System.IO
import Data.Char
import Data.List


-- PART B. STAND ALONE PROGRAMS

-- 2. columns
-- Write a program called columns that takes some numbers and a filename
-- as command-line arguments, and outputs the corresponding columns of
-- the input to standard output. Column N of a line is defined to be the
-- Nth item in a list which is obtained by splitting the line on
-- whitespace (starting from 1 for the first item).

main :: IO () 
main = do
    args <- getArgs
    (nums, filename) <- splitArgs args
    cols <- parseNums nums
    handle <- getHandle filename
    file <- hGetContents handle
    printCols (lines file) cols
    exitSuccess

printCols :: [String] -> [Int] -> IO ()
printCols lns cols = mapM_ printCol lns
    where 
        printCol :: String -> IO ()
        printCol l = 
            let line = words l :: [String] in
            let len = length line :: Int in
            let row = foldr (\i lst -> if i>len 
                then lst else line!!(i-1):lst) [] cols :: [String]
            in putStrLn $ intercalate " " row

getHandle :: String -> IO Handle
getHandle "-" = return stdin
getHandle filename = openFile filename ReadMode

splitArgs :: [String] -> IO ([String], String)
splitArgs args = case unsnoc args of
    Nothing -> usage >> exitFailure
    Just ([], _) -> usage >> exitFailure
    Just (cols, filename) -> return (cols, filename)

parseNums :: [String] -> IO ([Int])
parseNums nums = mapM parseNum nums
    where
        parseNum :: String -> IO Int
        parseNum x = 
            if not (all isDigit x) then usage >> exitFailure else
            let n = read x :: Int in 
            if n < 1 then usage >> exitFailure else
                return n

usage :: IO ()
usage = do 
    prog <- getProgName
    hPutStrLn stderr $ "usage: " ++ prog ++ " nl [n2 n3 ...] filename"
