-- Lab1.hs
module Lab1 where

import Prelude
import Data.List

-- PART A. EXERCISES

-- 1. Defining new operators

-- 1a. +* (sum of squares)
{-  Write a definition for an operator called +* that computes the sum 
    of the squares of its arguments. Assume both arguments are Doubles. 
    Make it left-associative and give it a precedence of 7. -}
(+*) :: Double -> Double -> Double
a +* b = a**2 + b**2 
infixl 7 +*

-- 1b. ^|| (exclusive-or)
{-  Write a definition for an operator called ^|| that computes the 
    exclusive-OR of its two (boolean) arguments. Make it 
    right-associative with a precedence of 3. Your definition should be 
    only two (very simple) lines (not counting the type declaration) and 
    shouldn't use if. -}
(^||) :: Bool -> Bool -> Bool
(^||) True a = not a
(^||) False a = a
infixr 3 ^||


-- 2. rangeProduct
{-  Write a recursive function called rangeProduct that takes two 
    Integers and computes the product of all the integers in the range 
    from one integer to the other (inclusive). -}
rangeProduct :: Integer -> Integer -> Integer
rangeProduct a b | a > b = 
    error "rangeProduct: first arg must be less than or equal to second arg"
rangeProduct a b | a == b = b
rangeProduct a b = a * rangeProduct (a + 1) b


-- 3. prod
{-  Use foldr to define a one-line point-free function called prod that 
    returns the product of all the numbers in a list of Integers (or 1 if 
    the list is empty). Use this function to define a one-line (not 
    including the type signature) non-recursive definition for 
    rangeProduct, which you should call rangeProduct2. (This will be a 
    two-line definition if you also include the line containing error.) -}
prod :: [Integer] -> Integer
prod lst = foldr (*) 1 lst

rangeProduct2 :: Integer -> Integer -> Integer
rangeProduct2 a b | a > b = 
    error "rangeProduct2: first arg must be less than or equal to second arg"
rangeProduct2 a b = prod [a..b]


-- 4. map and variations

-- 4a. map2
{-  Write a function map2 which maps a two-argument function over two 
    lists. Write it as a recursive function (there is a trivial 
    definition using zipWith which you shouldn't use). -}
map2 :: (a -> a -> b) -> [a] -> [a] -> [b]
map2 _ [] _ = []
map2 _ _ [] = []
map2 f (x:xs) (y:ys) = (f x y) : map2 f xs ys

-- 4b. map3
{-  Write a map3 function that will work for functions of 3 arguments. -}
map3:: (a -> a -> a -> b) -> [a] -> [a] -> [a] -> [b]
map3 _ [] _ _ = []
map3 _ _ [] _ = []
map3 _ _ _ [] = []
map3 f (x:xs) (y:ys) (z:zs) = (f x y z) : map3 f xs ys zs


-- 5. Dot product
{-  
Consider the following point-wise definition.
    dot :: [Integer] -> [Integer] -> Integer
    dot lst1 lst2 = sum (map2 (*) lst1 lst2)
Now, consider the correct point-free equivalent definition.
    dot :: [Integer] -> [Integer] -> Integer
    dot = (sum .) . map2 (*)
Use the following expansion for the . operator to show that dot lst1 lst2 
using the point-free definition of dot is equivalent to the point-wise 
definition. Start by expanding . as much as possible before applying it.
    f . g == \x -> f (g x)

Expanding . operators:
dot
(sum .) . map2 (*)
(\x -> sum . x) . map2 (*)
(\x -> (\y -> sum (x y))) . map2 (*)
\z -> (\x -> (\y -> sum (x y))) (map2 (*) z)
Thus, dot is equivalent to (\z -> (\x -> (\y -> sum (x y))) (map2 (*) z)).

Evaluation of dot lst1 lst2:
dot lst1 lst2       [Substitute dot]
(\z -> (\x -> (\y -> sum (x y))) (map2 (*) z)) lst1 lst2 
                    [Apply: Substitute z for lst1]
(\x -> (\y -> sum (x y))) (map2 (*) lst1) lst2 
                    [Apply: Substitute x for (map2 (*) lst1)]
(\y -> sum ((map2 (*) lst1) y)) lst2 
                    [Apply: Substitute y for lst2]
sum ((map2 (*) lst1) lst2)  
                    [Apply (map2 (*) lst1) to lst2]
sum (map2 (*) lst1 lst2)
Therefore, the point-free and point-wise definition are equivalent.


-}

-- 6. Fun with list comprehensions
{-  Using a list comprehension, write a one-line expression which computes 
    the sum of the natural numbers below one thousand which are multiples 
    of 3 or 5. Write the result in a comment. You may use the Haskell sum 
    function to do the actual sum. -}
res6 :: Integer
res6 = sum [x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0] 
-- result: 233168

-- 7. Sum of primes
{-  Calculate the sum of all the prime numbers below 10000. -}

{-  Generate an infinite list of prime numbers using the 
    "Sieve of Eratosthenes" algorithm. This consists of generating all 
    positive integers and removing all multiples of successive prime 
    numbers. Write a function called sieve which takes a list of integers, 
    retains the first one, removes all multiples of the first one from the 
    rest, and then sieves the rest. -}
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve (filter (\n -> n `mod` x /= 0) xs)

{-  Use takeWhile function on the primes list to take the appropriate 
    prime numbers and compute their sum. -}
primes :: [Integer]
primes = sieve [2..]

res7 :: Integer
res7 = sum $ takeWhile (< 10000) primes
-- result = 5736396


-- 8. Balanced Parentheses
{-  Write a function that takes a String and returns a Bool which is True 
    if the string has balanced parentheses and False otherwise. "Balanced" 
    means there are an equal number of open and close parentheses and a 
    close parenthesis never occurs before its matching open parenthesis.
    Write this function in 3 different ways. -}

-- 8a. balancedParentheses
-- Directly using a recursive helper function
balancedParentheses :: String -> Bool
balancedParentheses = iter 0 
    where 
        iter :: Integer -> String -> Bool 
        iter count [] = count == 0
        iter count _ | count < 0 = False
        iter count ('(':xs) = iter (count + 1) xs
        iter count (')':xs) = iter (count - 1) xs
        iter count (_:xs) = iter count xs

-- 8b. balancedParentheses2
-- Use foldl to count the final imbalance count of the string
balancedParentheses2 :: String -> Bool
balancedParentheses2 = (==) 0 . foldl' increment 0
    where 
        increment :: Int -> Char -> Int
        increment count _ | count < 0 = count
        increment count '(' = count + 1 
        increment count ')' = count - 1
        increment count _ = count

-- 8c. balancedParentheses3
{-  Efficiently using scanl and test function to map each character to an 
    Int and exit early if the imbalance count is ever negative -}
balancedParentheses3 :: String -> Bool
balancedParentheses3 = test . scanl' (+) 0 . map ctoi
    where 
        ctoi :: Char -> Int
        ctoi '(' = 1
        ctoi ')' = -1
        ctoi _ = 0

        test :: [Int] -> Bool
        test lst = all (>= 0) lst && (last lst) == 0
        


-- PART B. PITFALLS

-- 1. sumList
{-  
Bad:
    sumList :: [Integer] -> Integer
    sumList [] = 0
    sumList lst = head lst + sumList (tail lst)
This has poor Haskell style. Improve the style by making a simple 
change. Keep it a recursive definition but with better style. 
    
Corrected: 
    sumList :: [Integer] -> Integer
    sumList [] = 0
    sumList (x:xs) = x lst + sumList xs
In Haskell, it's better to pattern match on the head and tails of list as 
opposed to calling the head and tail functions when possible.
-}


-- 2. largest
{-  
Bad:
    -- Return the largest value in a list of integers.
    largest :: [Integer] -> Integer
    largest xs | length xs == 0 = error "empty list"
    largest xs | length xs == 1 = head xs
    largest xs = max (head xs) (largest (tail xs))
This recursive definition works but something is wrong with it. Fix it 
while keeping it a recursive definition.
    
Corrected: 
    -- Return the largest value in a list of integers.
    largest :: [Integer] -> Integer
    largest [] = error "empty list"
    largest [x] = head x
    largest (x:xs) = max x (largest xs)
Again, this is a pattern matching issue. Using pattern guards and the 
head and tail functions when pattern matching is possible is bad style, 
potentially inefficient, and discouraged.
-}
       


-- PART C. EVALUATION

-- 1. Fibonacci
{- 
Consider the following function and evaluate fib 3.
    fib :: Integer -> Integer
    fib 0 = 0
    fib 1 = 1
    fib n = fib (n - 1) + fib (n - 2)

fib 3                                   [Expand fib 3 from def]
fib (3 - 1) + fib (3 - 2)               [Evaluate (3 - 1)]
fib 2 + fib (3 - 2)                     [Expand fib 2 from def]
fib (2 - 1) + fib (2 - 2) + fib (3 - 2) [Evaluate (2 - 1)]
fib 1 + fib (2 - 2) + fib (3 - 2)       [Reduce fib 1 from def]
1 + fib (2 - 2) + fib (3 - 2)           [Evaluate (2 - 2)]
1 + fib 0 + fib (3 - 2)                 [Reduce fib 0 from def]
1 + 0 + fib (3 - 2)                     [Reduce fib 0 from def]
1 + 0 + fib 1                           [Evaluate (3 - 2)]
1 + 0 + 1                               [Reduce fib 1 from def]
1 + 1                                   [Evaluate 1 + 0, + left-associative]
2                                       [Evaluate 1 + 1]
Thus, fib 3 = 2.
-}



-- 2. Factorial
{- 
The following function is wrong. Fix it and then 
evaluate fact 3 from fixed definition.
    fact :: Integer -> Integer
    fact n = n * fact (n - 1)
    fact 0 = 1

Fixed:
    fact :: Integer -> Integer
    fact 0 = 1
    fact n = n * fact (n - 1)
The fact 0 is never pattern matched if fact n definition comes first.

Evaluation of incorrect fact 3:
fact 3
3 * fact (3 - 1) 
-- Grading note:
    I lost a point on this part previously. The comment said "3-1 doesn't
    need to evaluate to 2." However, this is necessary because fact needs to
    know the result in order to pattern match it to 0 or n and continue the
    evaluation.
3 * fact 2
3 * 2 * fact (2 - 1)
3 * 2 * fact 1
3 * 2 * 1 * fact (1 - 1)
3 * 2 * 1 * fact 0
3 * 2 * 1 * 0 * fact (0 - 1)
3 * 2 * 1 * 0 * fact (-1)
3 * 2 * 1 * 0 * (-1) * fact (-1 - 1)
3 * 2 * 1 * 0 * (-1) * fact (-2)
3 * 2 * 1 * 0 * (-1) * (-2) * fact (-2 - 1)
This will repeat infinitely resulting in a Stack Overflow error.

Evaluation of fixed fact 3:
fact 3
3 * fact (3 - 1) 
3 * fact 2
3 * 2 * fact (2 - 1)
3 * 2 * fact 1
3 * 2 * 1 * fact (1 - 1)
3 * 2 * 1 * fact 0
3 * 2 * 1 * 1
6 * 1 * 1
6 * 1
6
Thus, fact 3 = 6.
-}


-- 3. reverse
{- 
Consider the following function and evaluate reverse [1,2,3]. 
Also, find its asymptotic time complexity (as a function of the length 
of the input list) and explain why it's correct.
    reverse :: [a] -> [a]
    reverse xs = iter xs []
    where
        iter :: [a] -> [a] -> [a]
        iter [] ys = ys
        iter (x:xs) ys = iter xs (x:ys)

reverse [1,2,3]
iter [1,2,3] []
iter [2,3] (1:[])
iter [3] (2:1:[])
iter [] (3:2:1:[])
3:2:1:[]
3:2:[1]
3:[2,1]
[3,2,1]
Thus, reverse [1,2,3] = [3,2,1].

Asymptotic Time Complexity: O(n)
The function removes each of the n items from the list and appends them 
to the front of a new list, requiring an iteration per move. Each of these 
moves has constant time complexity, so the overall complexity is O(n). 
-}


-- 4. reverse again
{- 
Consider the following function and evaluate reverse [1,2,3]. 
    reverse :: [a] -> [a]
    reverse [] = []
    reverse (x:xs) = reverse xs ++ [x]
For reference, the following is the definition of the (++) operator, and 
the (++) operator is right-associative.
    (++) :: [a] -> [a] -> [a]
    (++) []     ys = ys
    (++) (x:xs) ys = x : (xs ++ ys)
Ben Bitfiddle claims this definition of reverse has an asymptotic time 
complexity which is linear in the length of the input list, giving this 
argument: "Evaluating reverse [1, 2, 3] eventually results in 
[] ++ [3] ++ [2] ++ [1] after a linear number of steps, and since 
appending a singleton list to another list is an O(1) operation, 
constructing the result list from this point is also linear." 
Explain what is wrong this argument and where and what Ben's mistake was.

Evaluation of reverse [1,2,3]:
reverse [1,2,3]
reverse [2,3] ++ [1]
reverse [3] ++ [2] ++ [1]
reverse [] ++ [3] ++ [2] ++ [1]
[] ++ [3] ++ [2] ++ [1]
[] ++ [3] ++ 2 : ([] ++ [1])
[] ++ [3] ++ 2 : [1]
[] ++ [3] ++ [2,1]
[] ++ 3 : ([] ++ [2,1])
[] ++ 3 : [2,1]
[] ++ [3,2,1]
[3,2,1]
Thus, reverse [1,2,3] = [3,2,1].

Ben's mistake:
Although it takes a linear number of steps to get from reverse [1,2,3] to 
[] ++ [3] ++ [2] ++ [1], getting from [] ++ [3] ++ [2] ++ [1] to the final 
result is not linear in time. This is where Ben made a mistake because 
appending the singleton lists together actually requires O(n^2) time, as 
can be seen above. Thus, the actual asymptotic time complexity of this 
reverse in O(n) + O(n^2) = O(n^2). 
-}


-- 5. Insertion sort
{- 
Consider the following function and evaluate head (isort [3, 1, 2, 5, 4]).
    isort :: [Integer] -> [Integer]
    isort [] = []
    isort (n:ns) = insert n (isort ns)
    where
        insert :: Integer -> [Integer] -> [Integer]
        insert n [] = [n]
        insert n m@(m1:_) | n < m1 = n : m
        insert n (m1:ms) = m1 : insert n ms
For reference, the following is the definition of the head function.
    head :: [a] -> a
    head [] = error "empty list"
    head (x:_) = x

head (isort [3,1,2,5,4])    
head (insert 3 (isort ([1,2,5,4]))) 
head (insert 3 (insert 1 (isort [2,5,4]))) 
head (insert 3 (insert 1 (insert 2 (isort [5,4])))) 
head (insert 3 (insert 1 (insert 2 (insert 5 (isort [4]))))) 
head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 (isort [])))))) 
head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 []))))) 
head (insert 3 (insert 1 (insert 2 (insert 5 [4])))) 
head (insert 3 (insert 1 (insert 2 (4 : insert 5 []))))
head (insert 3 (insert 1 (insert 2 (4 : [5]))))
head (insert 3 (insert 1 (insert 2 [4, 5])))
head (insert 3 (insert 1 (2 : [4, 5])))
head (insert 3 (insert 1 [2, 4, 5]))
head (insert 3 (1 : [2, 4, 5]))
head (insert 3 [1, 2, 4, 5])
head (insert 3 [1, 2, 4, 5])
head (1 : insert 3 [2, 4, 5])
head (1 : 2 : insert 3 [4, 5])
head (1 : 2 : 3 : [4, 5])
head (1 : 2 : [3, 4, 5])
head (1 : [2, 3, 4, 5])
head [1, 2, 3, 4, 5]
1
Thus, head (isort [3,1,2,5,4]) = 1.
-}


-- 6. foldr and foldl
{- 
Consider the following definitions of foldr and foldl. 
(Note that these are not the actual definitions in ghc)
foldr:
    foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr _ init [] = init
    foldr f init (x:xs) = f x (foldr f init xs)
foldl:
    foldl :: (a -> b -> a) -> a -> [b] -> a
    foldl _ init [] = init
    foldl f init (x:xs) = foldl f (f init x) xs
Also, note that max returns the max of two Nums.
Now, evaluate the following expressions and compare the space complexity 
of foldr to foldl.

Evaluation of foldr max 0 [1, 5, 3, -2, 4]:
foldr max 0 [1,5,3,-2,4]
max 1 (foldr max 0 [5,3,-2,4])
max 1 (max 5 (foldr max 0 [3,-2,4]))
max 1 (max 5 (max 3 (foldr max 0 [-2,4])))
max 1 (max 5 (max 3 (max -2 (foldr max 0 [4]))))
max 1 (max 5 (max 3 (max -2 (max 4 (foldr max 0 [])))))
max 1 (max 5 (max 3 (max -2 (max 4 0))))
max 1 (max 5 (max 3 (max -2 4)))
max 1 (max 5 (max 3 4))
max 1 (max 5 4)
max 1 5
5
Thus, foldr max 0 [1,5,3,-2,4] = 5.
It's clear foldr has O(n) asymptotic space complexity.

Evaluation of foldl max 0 [1, 5, 3, -2, 4]:
foldl max 0 [1,5,3,-2,4]
foldl max (max 0 1) [5,3,-2,4]
foldl max (max (max 0 1) 5) [3,-2,4]
foldl max (max (max (max 0 1) 5) 3) [-2,4]
foldl max (max (max (max (max 0 1) 5) 3) -2) [4]
foldl max (max (max (max (max (max 0 1) 5) 3) -2) 4) []
max (max (max (max (max 0 1) 5) 3) -2) 4
max (max (max (max 1 5) 3) -2) 4
max (max (max 5 3) -2) 4
max (max 5 -2) 4
max 5 4
5

Thus, foldl max 0 [1,5,3,-2,4] = 5.
It's clear foldl has O(n) asymptotic space complexity.

Therefore, foldr and foldl both have the same O(n) aymptotic space 
complexity. This is because all of the f's are computed at the end after 
the list has been fully expanded. For foldr, f doesn't receive two
computed arguments until the list has been fully expanded, so it is 
impossible for f to be computed until that point. In the case of foldl, 
however, f does have the computed arguments necessary to compute at
intermediate points, but due to lazy evaluation, Haskell doesn't do this 
computation. Doing intermediate computations of f would save a lot of 
space. This is why foldl' is often used instead to force strict evaluation.
-}