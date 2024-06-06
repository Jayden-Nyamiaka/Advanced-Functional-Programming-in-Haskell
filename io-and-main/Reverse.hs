-- Reverse.hs
module Main where

import Prelude
import System.Environment
import System.Exit
import System.IO


-- PART B. STAND ALONE PROGRAMS

-- 1. reverse
-- Write a program called reverse that reads in all the lines of a file
-- and prints them, in reverse order, to standard output.

main :: IO () 
main = do
    args <- getArgs
    filename <- parseArgs args
    file <- readFile filename
    mapM_ putStrLn $ reverse $ lines file
    exitSuccess

parseArgs :: [String] -> IO String
parseArgs [first] = return first
parseArgs _ = usage >> exitFailure

usage :: IO ()
usage = do 
    prog <- getProgName
    hPutStrLn stderr $ "usage: " ++ prog ++ " filename"
    hPutStrLn stderr $ "  filename: string for input file"