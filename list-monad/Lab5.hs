-- Lab5.hs
module Lab5 where

import Prelude
import Control.Monad


-- PART A. THE LIST MONAD


-- 1. The Hardy-Ramanujan problem
{-  A classic problem in number theory is to find positive integers that can
    be expressed as the sum of two cubes in two different ways. This problem
    dates back to the mathematicians Hardy and Ramanujan; supposedly Ramanujan
    could tell at a glance that 1729 was the smallest such integer. -}
-- Via List Comprehension
hr_solutions :: [((Integer, Integer), (Integer, Integer), Integer)]
hr_solutions =
    [((i, l), (j, k), i^3 + l^3) |
    i <- [1..],
    j <- [1..i-1],
    k <- [1..j-1],
    l <- [1..k-1],
    i^3 + l^3 == j^3 + k^3]
hr_solutions_take_5 = take 5 hr_solutions
-- Via List Monad
hr_solutions2 :: [((Integer, Integer), (Integer, Integer), Integer)]
hr_solutions2 = do
    i <- [1..]
    j <- [1..i-1]
    k <- [1..j-1]
    l <- [1..k-1]
    guard $ i^3 + l^3 == j^3 + k^3
    return ((i, l), (j, k), i^3 + l^3)
hr_solutions2_take_5 = take 5 hr_solutions2


-- 2. Redo Exercise 1A.6
-- Write an expression which computes the sum of the natural numbers below 
-- one thousand which are multiples of 3 or 5.
-- Via List Comprehension
mult35 :: Integer
mult35 = sum [x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0]
-- Via List Monad with guard
mult35_1 :: Integer
mult35_1 = sum $ do
    x <- [1..999]
    guard $ mod x 3 == 0 || mod x 5 == 0
    return x
-- Via List Monad with mzero
mult35_2 :: Integer
mult35_2 = sum $ do
    x <- [1..999]
    if mod x 3 == 0 || mod x 5 == 0
        then return x
        else mzero
-- result for all = 233168


-- 3. Project Euler problem #4
{-  A palindromic number reads the same both ways. The largest palindrome
    made from the product of two 2-digit numbers is 9009 = 91 * 99. Find
    the largest palindrome made from the product of two 3-digit numbers. -}
isPalindrome :: Integer -> Bool
isPalindrome n = (show n) == (reverse $ show n)
largestPalindrome :: Integer
largestPalindrome = maximum $ do
    x <- [100..999]
    y <- [x..999]
    guard $ isPalindrome $ x * y
    return $ x * y
-- largestPalindrome = 906609


-- 4. A combinatorial puzzle
{-  Take the digits 1 to 9 in sequence. Put either a +, a -, or nothing (i.e.
    concatenation of 2 digits) between each digit to get an arithmetic
    expression. Print out all such expressions that evaluate to 100. -}
type Expr = [Item]

data Item = N Int | O Op
  deriving Show

data Op = Add | Sub | Cat
  deriving Show

-- Consists of all operators
ops :: [Item]
ops = [O Add, O Sub, O Cat]

-- Consists of all possible valid expressions from the puzzle description
exprs :: [Expr]
exprs = do
    o1 <- ops
    o2 <- ops
    o3 <- ops
    o4 <- ops
    o5 <- ops
    o6 <- ops
    o7 <- ops
    o8 <- ops
    return [N 1,o1,N 2,o2,N 3,o3,N 4,o4,N 5,o5,N 6,o6,N 7,o7,N 8,o8,N 9]

-- Takes an expression and removes all instances of the Cat operator by
-- applying the transformation: N i, Cat, N j --> N (ij) anywhere in the list
normalize :: Expr -> Expr
normalize [N i] = [N i]
normalize (N i: O Cat: N j: t) = normalize $ N (i * 10 + j) : t
normalize (N i: O Add: N j: t) = N i: O Add : (normalize $ N j: t)
normalize (N i: O Sub: N j: t) = N i: O Sub : (normalize $ N j: t)
normalize _ = error "normalize: invalid arithmetic expression"

-- Takes a normalized expression (i.e. one with no Cat operators) and
-- evaluates it to return the computed Int
evaluate :: Expr -> Int
evaluate [N i] = i
evaluate (N i: O Add: N j: t) = evaluate $ N (i + j) : t
evaluate (N i: O Sub: N j: t) = evaluate $ N (i - j) : t
evaluate (N _: O Cat: t) = 
    error "evaluate: expr must be normalized (i.e. no concats)"
evaluate _ = 
    error "evaluate: invalid expr (violates invariants)"

-- Pick out the expressions that evaluate to a particular number.
find :: Int -> [Expr] -> [Expr]
find n = filter (\e -> evaluate (normalize e) == n)

-- Pretty-print an expression.
pprint :: Expr -> String
pprint [N i] = show i
pprint (N i : O Add : es) = show i ++ " + " ++ pprint es
pprint (N i : O Sub : es) = show i ++ " - " ++ pprint es
pprint (N i : O Cat : es) = show i ++ pprint es
pprint _ = error "pprint: invalid argument"

-- Run the computation and print out the answers.
run :: IO ()
run = mapM_ putStrLn $ map pprint $ find 100 exprs



-- PART B. PUZZLES AND DERIVATIONS
{-  We will solve the following 3 puzzles by desugaring the do notation into 
    return, >>=, and >> and then substituting the List Monad definitions. We
    will solve the last puzzle by further considering >>= for the List Monad.
    This will grant us knowledge behind what's happening behind the scenes in
    the List Monad. Note that the order of reduction is not important. -}


-- 1. [] in the List Monad
-- The following evaluates to []. Desugar and derive to see why.
res1 = do 
    n1 <- [1..6]
    n2 <- [1..6]
    []
    return (n1, n2)
-- res1 = []
{- 
-- Desugar do notation
[1..6] >>= \n1 -> [1..6] >>= \n2 -> [] >> return (n1, n2)
-- Substitute List Monad Definitions
[1..6] >>= \n1 -> [1..6] >>= \n2 -> [] >> return [(n1, n2)]
concatMap (\n1 -> [1..6] >>= \n2 -> [] >> [(n1, n2)]) [1..6]
concatMap (\n1 -> concatMap (\n2 -> [] >>= \_ -> [(n1, n2)]) [1..6]) [1..6]
concatMap (\n1 -> concatMap (\n2 -> concatMap (\_ -> [(n1, n2)]) []) [1..6]) [1..6]
-}
{-  Since concatMap <anything> [] = [], concatMap (\_ -> [(n1, n2)]) [] = [].
    Then, all subsequent functions will have the form (\n -> []), and since
    concatMap (\n -> []) <anything> = [], the final result is []. This is
    why guard from Control.Monad works for list monad expressions. -}


-- 2. The case of the useless return
-- The 2 following expressions return the same thing.
-- Desugar and derive to see why.
{-
-- Expression a
res2a = do 
    n1 <- [1..6]
    n2 <- [1..6]
    return <anything>
    return (n1, n2)
-- Expression b
res2b = do 
    n1 <- [1..6]
    n2 <- [1..6]
    return (n1, n2)
-}

{-
-- Expression a
-- Desugar do notation
[1..6] >>= \n1 -> [1..6] >>= \n2 -> return <anything> >> return (n1, n2)
-- Substitute List Monad Definitions
[1..6] >>= \n1 -> [1..6] >>= \n2 -> [<anything>] >> [(n1, n2)]
concatMap (\n1 -> concatMap (\n2 -> [<anything>] >> [(n1, n2)]) [1..6]) [1..6]
concatMap (\n1 -> concatMap (\n2 -> [<anything>] >>= \_ -> [(n1, n2)]) [1..6]) [1..6]
concatMap (\n1 -> concatMap (\n2 -> concatMap (\_ -> [(n1, n2)]) [<anything>]) [1..6]) [1..6]
-- Further evaluation of concatMap (\_ -> [(n1, n2)]) [<anything>]
concatMap (\_ -> [(n1, n2)]) [<anything>]
concat (map (\_ -> [(n1, n2)]) [<anything>])
concat [(\_ -> [(n1, n2)]) <anything>]
concat [[(n1, n2)]]
[(n1, n2)]
-- Plug this back into larger expression
concatMap (\n1 -> concatMap (\n2 -> [(n1, n2)]) [1..6]) [1..6]
-- ^ Final Expression for Expression a

-- Expression b
-- Desugar do notation
[1..6] >>= \n1 -> [1..6] >>= \n2 -> return (n1, n2)
-- Substitute List Monad Definitions
[1..6] >>= \n1 -> [1..6] >>= \n2 -> [(n1, n2)]
concatMap (\n1 -> concatMap (\n2 -> [(n1, n2)]) [1..6]) [1..6]
-- ^ Final Expression for Expression b

-- Both expressions reduce to the same expression, evincing their equality.
-}


-- 3. A trivial pattern-matcher
{-  List monad can also perform simple pattern matching. Consider the 
    following code and desugar and derive the result.
let s = ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] in
    do ['a', 'a', c1, c2, 'b', 'b'] <- s
        return [c1, c2]
-- returns ["xy", "zw", "cc"]
    Note the fail method of the MonadFail type class has the definition in the
    List Monad: fail _ = []. If it instead had definition fail s = error s,
    what would happen? Would to code still work? Why or why not? -}
{-
-- Desugar do notation
let s = ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] in
    s >>= 
        \y -> case y of
            ['a', 'a', c1, c2, 'b', 'b'] -> return [c1, c2]
            _ -> fail "some string"
-- Substitute List Monad Definitions
let s = ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] in
    concatMap (\y -> case y of
        ['a', 'a', c1, c2, 'b', 'b'] -> [[c1, c2]]
        _ -> fail "some string") s
-- Substitute s
concatMap (\y -> case y of
    ['a', 'a', c1, c2, 'b', 'b'] -> [[c1, c2]]
    _ -> fail "some string") 
    ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"]
-- Substitute fail _ = []
concatMap (\y -> case y of
    ['a', 'a', c1, c2, 'b', 'b'] -> [[c1, c2]]
    _ -> []) 
    ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"]
{-  This will pattern match all Strings in s using the lambda function. If the
    the string matches the pattern "aa__bb", then the function returns the 2
    middle characters as a string. Otherwise, it returns the empty String [].
    Then, these results get concatenated together to return 
    ["xy", "zw", "cc"]. This is why this works. However, we can consider if
    fail had the alternative definition fail s = error s below. -}
-- Substitute fail s = error s
concatMap (\y -> case y of
    ['a', 'a', c1, c2, 'b', 'b'] -> [[c1, c2]]
    _ -> error "some string") 
    ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"]
{-  In this case, all strings that don't match the pattern "aa__bb" would
    result in an error. Here, "foobar" and "baz" don't match the pattern,
    so the code would result in an error and this would not work. This
    illustrates why fail returns [] for the list monad. -}
-}


-- 4. An equivalence
-- The actual GHC definition for >>= is 
-- m >>= k = foldr ((++) . k) [] m
-- However, an equivalent alternative definition for >>= is
-- m >>= k = concat (map k m)
-- We will show these definitions are equivalent.

{-
m >>= k = foldr ((++) . k) [] m
-- Converting (++) . k into lambda expression, we get
m >>= k = foldr (\x -> (++) (k x)) [] m

-- Now, we can show the 2 definitions are equivalent by showing they evaluate
-- m = [] and m = [x1, x2, ..., xn] to the same expression

-- Case m = []
-- Definition 1
m >>= k = foldr (\x -> (++) (k x)) [] []
m >>= k = [] -- nothing to fold over
-- Definition 2
m >>= k = concat (map k [])
m >>= k = [] -- nothing to concat map over

-- Case m = [x1, x2, ..., xn]
-- Definition 1
m >>= k = foldr (\x -> (++) (k x)) [] [x1, x2, ..., xn]
m >>= k = (\x -> (++) (k x)) x1 $ (\x -> (++) (k x)) x2 $ ... (\x -> (++) (k x)) xn []
m >>= k = (++) (k x1) $ (++) (k x2) $ ... (++) (k xn) []
m >>= k = (++) (k x1) $ (++) (k x2) $ ... (++) (k xn) []
-- Definition 2
m >>= k = concat (map k [x1, x2, ..., xn])
m >>= k = concat [k x1, k x2, ..., k xn]
-- Definition 1 adds all the elements from each k xi to the front of an empty
-- list starting from the last element to the first one, thus concatenating
-- all k xi's. Similarly, definition 2 concatenates all results for each k xi.
-- Thus, the result for both of these definitions is the same, returning a
-- list with all elements in each k xi concatenated in order.
-}
