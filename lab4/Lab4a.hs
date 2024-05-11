-- Lab4a.hs
module Lab4a where

import Prelude
import Data.Char

-- PART A. DESUGARING EXERCISES


-- 1. Desugaring do
-- Desugar this into a function that does not use the do notation.
myPutStrLn :: String -> IO ()
myPutStrLn "" = putChar '\n'
myPutStrLn (c:cs) = do 
    putChar c
    myPutStrLn cs

-- Pattern-match is exhaustive, so Haskell uses Way 1
myPutStrLn2 :: String -> IO ()
myPutStrLn2 "" = putChar '\n'
myPutStrLn2 (c:cs) =
    putChar c >> myPutStrLn2 cs


-- 2. A style mistake
-- This code has some redundancy; simplify it without changing its behavior.
greet :: String -> IO ()
greet name = do putStrLn ("Hello, " ++ name ++ "!")

-- Can be simplified into 
greetS :: String -> IO ()
greetS name = putStrLn $ "Hello, " ++ name ++ "!"


-- 3. Two desugarings
-- Desugar this using both the simple and more complicated way.

-- Ask the user for his/her name, then print a greeting.
greet2 :: IO ()
greet2 = do
    putStr "Enter your name: "
    name <- getLine
    putStr "Hello, "
    putStr name
    putStrLn "!"

-- Way 1 (Simple)
greet2a :: IO ()
greet2a = 
    putStr "Enter your name: " >> getLine >>= 
        \name -> putStr "Hello, " >> putStr name >> putStrLn "!"

-- Way 2 (More complicated)
greet2b :: IO ()
greet2b = do
    putStr "Enter your name: " >> getLine >>= 
        \y -> case y of 
            name -> putStr "Hello, " >> putStr name >> putStrLn "!"
            _ -> fail "Pattern match failure in do expression"
{-  The complex desugaring has the same behavior. It introduces an
    additional pattern match to getLine, but the case is redundant and
    will never be matched. This makes the behavior exactly the same. -}


-- 4. Two desugarings, part 2
-- Desugar this using both the simple and more complicated way.

-- Ask the user for his/her name, then print a greeting.
-- Capitalize the first letter of the name.
greet3 :: IO ()
greet3 = do
    putStr "Enter your name: "
    (n:ns) <- getLine
    let name = toUpper n : ns
    putStr "Hello, "
    putStr name
    putStrLn "!"

-- Way 1 (Simple)
greet3a :: IO ()
greet3a = do
    putStr "Enter your name: " >> getLine >>=
        \(n:ns) -> 
            let name = toUpper n : ns in
                putStr "Hello, " >> putStr name >> putStrLn "!"

-- Way 2 (More complicated)
greet3b :: IO ()
greet3b = do
    putStr "Enter your name: " >> getLine >>=
        \y -> case y of 
            (n:ns) -> 
                let name = toUpper n : ns in
                putStr "Hello, " >> putStr name >> putStrLn "!"
            _ -> fail "Pattern match failure in do expression"
{-  The complex desugaring does have another effect here. It accounts for
    the partial pattern match of getLine with (n:ns). The pattern (n:ns)
    is not exhaustive; if the user inputs an empty list [], (n:ns) is not
    matched, and there will be an error using the first desugaring. However,
    the second desugaring accounts for this by adding a catch all pattern
    at the end that will manually fail (using MonadFail) in the case the
    user inputs an empty list. The second desugaring is used specifically
    for non-exhaustive pattern match cases like this. -}
            

