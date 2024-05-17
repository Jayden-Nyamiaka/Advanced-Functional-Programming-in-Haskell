-- Lab3ab.hs
module Lab3ab where

import Prelude


-- PART A. TYPE CLASS EXERCISES


-- Custom type definition for Natural Numbers
data Nat = Zero | Succ Nat
  deriving (Eq, Show, Ord)

-- 1. Eq and Show for Nat1 (manual definitions)
data Nat1 = Zero1 | Succ1 Nat1
instance Eq Nat1 where 
    (==) = nat1Eq
        where 
            nat1Eq :: Nat1 -> Nat1 -> Bool
            nat1Eq Zero1 Zero1 = True
            nat1Eq Zero1 _ = False
            nat1Eq _ Zero1 = False
            nat1Eq (Succ1 a) (Succ1 b) = nat1Eq a b
instance Show Nat1 where 
    show Zero1 = "Zero1"
    show (Succ1 Zero1) = "Succ1 Zero1"
    show (Succ1 n) = "Succ1 (" ++ show n ++ ")"

-- 2. Eq and Show for Nat2 (derived, automatic definitions)
data Nat2 = Zero2 | Succ2 Nat2
    deriving (Eq, Show)

-- 3. Ord for Nat2
instance Ord Nat2 where
    (<=) = nat2Leq
        where 
            nat2Leq :: Nat2 -> Nat2 -> Bool
            nat2Leq Zero2 _ = True
            nat2Leq _ Zero2 = False
            nat2Leq (Succ2 a) (Succ2 b) = nat2Leq a b
{-  Deriving the Ord type class will result in an automatic definition that
    is functionally equivalent to this one. This is because instances of the
    Ord type class that are derived order by constructor location. Thus, 
    Succ2 Zero2 is considered greater than Zero2, and when comparing any two
    Succ2s, the Succ2 with the longer chain will be considered greater
    because it has a longer chain of constructors. You can observe this 
    directly by making Succ2 derive Ord for yourself. -} 

-- 4. Eq and Ord for SignedNat
data SignedNat =
  Neg Nat | Pos Nat
  deriving Show
instance Eq SignedNat where 
    (==) = signedNatEq
        where 
            signedNatEq :: SignedNat -> SignedNat -> Bool
            signedNatEq (Pos Zero) (Neg Zero) = True
            signedNatEq (Neg Zero) (Pos Zero) = True
            signedNatEq (Pos a) (Pos b) = a == b
            signedNatEq (Neg a) (Neg b) = a == b
            signedNatEq _ _ = False
instance Ord SignedNat where 
    (<=) = signedNatLeq
        where 
            signedNatLeq :: SignedNat -> SignedNat -> Bool
            signedNatLeq a b | a == b = True
            signedNatLeq (Neg Zero) (Pos _) = True
            signedNatLeq (Neg Zero) (Neg _) = False
            signedNatLeq (Pos _) (Neg Zero) = False
            signedNatLeq (Neg _) (Neg Zero) = True
            signedNatLeq (Neg a) (Neg b) = a >= b
            signedNatLeq (Neg _) _ = True
            signedNatLeq (Pos a) (Pos b) = a <= b
            signedNatLeq (Pos _) _ = False
{-  In this case, we do not have the option of deriving Eq and Ord to 
    generate automatic definitions because of Zero. The automatic instances
    would consider Neg less than Pos since that is the order of their
    constructors. However, Neg Zero and Pos Zero should be equal. The two
    representations of Zero make the automatically-derived definitions
    differ from the functionality we desire. -}

-- Helper arithmetic functions for Nat
natAdd :: Nat -> Nat -> Nat
natAdd a Zero = a
natAdd a (Succ b) = natAdd (Succ a) b

natAbs :: Nat -> Nat -> Nat
natAbs a Zero = a
natAbs Zero b = b
natAbs (Succ a) (Succ b) = natAbs a b

natMult :: Nat -> Nat -> Nat
natMult _ Zero = Zero
natMult Zero _ = Zero
natMult (Succ Zero) b = b
natMult (Succ a) b = natAdd (natMult a b) b 

-- 5. Num for SignedNat
instance Num SignedNat where 
    --negate (Pos Zero) = Pos Zero
    negate (Neg a) = Pos a
    negate (Pos a) = Neg a

    abs (Neg a) =  Pos a
    abs a = a

    signum (Pos Zero) = Pos Zero
    signum (Neg Zero) = Pos Zero
    signum (Neg _) = Neg (Succ Zero)
    signum _ = Pos (Succ Zero)

    (+) = signedNatAdd
        where
            signedNatAdd :: SignedNat -> SignedNat -> SignedNat
            signedNatAdd (Pos a) (Pos b) = Pos (natAdd a b)
            signedNatAdd (Neg a) (Neg b) = Neg (natAdd a b)

            signedNatAdd (Pos a) (Neg b) | a == b = Pos Zero
            signedNatAdd (Pos a) (Neg b) | a > b = Pos (natAbs a b)
            signedNatAdd (Pos a) (Neg b) = Neg (natAbs a b)

            signedNatAdd (Neg a) (Pos b) | a == b = Pos Zero
            signedNatAdd (Neg a) (Pos b) | a > b = Neg (natAbs a b)
            signedNatAdd (Neg a) (Pos b) = Pos (natAbs a b)

    (*) = signedNatMult
        where 
            signedNatMult :: SignedNat -> SignedNat -> SignedNat
            signedNatMult (Pos a) (Pos b) = Pos (natMult a b)
            signedNatMult (Neg a) (Neg b) = Pos (natMult a b)
            signedNatMult (Pos a) (Neg b) = Neg (natMult a b)
            signedNatMult (Neg a) (Pos b) = Neg (natMult a b)

    fromInteger = signedNatFromInteger
        where
            signedNatFromInteger :: Integer -> SignedNat
            signedNatFromInteger n | n < 0 = Neg (posFromIntegerAccum (-n) Zero)
            signedNatFromInteger n = Pos (posFromIntegerAccum n Zero)
            posFromIntegerAccum :: Integer -> Nat -> Nat
            posFromIntegerAccum 0 sn = sn
            posFromIntegerAccum n sn = posFromIntegerAccum (n - 1) (Succ sn)

-- 6. signedNatToInteger
natToInteger :: Nat -> Integer
natToInteger a = natToIntegerAccum a 0
    where 
        natToIntegerAccum :: Nat -> Integer -> Integer
        natToIntegerAccum Zero n = n
        natToIntegerAccum (Succ a) n = natToIntegerAccum a (n + 1)
signedNatToInteger :: SignedNat -> Integer
signedNatToInteger (Pos n) = natToInteger n
signedNatToInteger (Neg n) = -1 * natToInteger n

-- 7. UnaryInteger
data UnaryInteger = UDecr UnaryInteger | UZero | USucc UnaryInteger
{-  I disliked the SignedNat definition of integers because there was no
    inherent, intuitive order between positive and negative numbers, and
    there were 2 representations of 0. UnaryInteger, on the other hand,
    has an obvious, most intuitive way to express 0, and there is an
    intuitive ordering. However, this makes it possible to nest UDecr and
    USucc, which goes against the desired use case and technically makes
    it possible to express every integer in an infinite number of ways. It
    would be ideal if we could make Haskell enforce the invariants that
    UDecr can only hold Zero or UDecr and USucc can only hold Zero or 
    USucc, but Haskell does not have this capability. Thus, it is up to
    the user to abide by these invariants themselves, which is a tradeoff
    we accept in our design of UnaryInteger. -}

-- 8. Generic factorial
factorial :: (Num a, Ord a) => a -> a
factorial n | abs n /= n = error "factorial: no negative numbers"
factorial n | n == fromInteger 1 = fromInteger 1
factorial n = n * factorial (n - fromInteger 1)
-- Testing generic factorial
testFactRes = factorial (Pos (Succ (Succ (Succ Zero))))
-- testFactRes = Pos (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))


-- PART B. OPERATORS


-- 1. Operator Associativity Exercises
{-  
For each of the following operators, indicate what the associativity 
(fixity) should be. Choices are...
    - If neither infixr or infixl will work with chained operator
    expressions (i.e. both of them lead to type errors), declare it to
    be infix (non-associative).
    - If the operator can be declared either infixr or infixl without
    resulting in a type error for chained operator expressions, declare
    it infixl but add a comment stating that it could be infixr as well.
    - If the operator can be declared as only one of infixr or infixl
    without resulting in a type error for chained operator expressions,
    declare it to be the correct associativity (either infixr or infixl;
    whichever works).
If an operator can be left-associative, right-associative, or both, give
one example of a chained operator expression that works. No examples are
needed if the operator is non-associative.
-}

{- The operator >#< compares two integer scores and tells you the winner
    (as a string). For example, 51 >#< 40 = "First Player", and
    21 >#< 21 = "Tie". It has type Integer -> Integer -> String. -}
-- (>#<) :: Integer -> Integer -> String
-- >#< infix (non-associative)
{-  Explanation: The function takes 2 Integers as input and returns a
    String, but it can't take the String as input into itself for
    successive applications. -}

{- The operator +| adds two integers and takes the last digit of their sum.
    For example, 7 +| 6 = 3. It has type Integer -> Integer -> Integer. -}
-- (+|) :: Integer -> Integer -> Integer
-- +| infixl (can also be infixr)
-- Example with left-associativity: 7 +| 6 +| 8 = (7 +| 6) +| 8 = 1
-- Example with right-associativity: 7 +| 6 +| 8 = 7 +| (6 +| 8) = 1
{-  Explanation: Function takes 2 Integers as input and returns an Integer,
    so the returned Integer can be either its left or right argument. -}

{-  The operator &< appends an integer to the end of a list. For example,
    [1, 2] &< 3 = [1, 2, 3]. It has type [Integer] -> Integer -> [Integer],
    where [Integer] means a list of integers. -}
-- (&<) :: [Integer] -> Integer -> [Integer]
-- &< infixl 
-- Example: [1, 2] &< 3 &< 4 = ([1, 2] &< 3) &< 4 = [1, 2, 3, 4]
{-  Explanation: Function takes [Integer] and Integer as input and returns
    [Integer], so it can only take [Integer] as its first arg s.t. it must be
    left-associative for [Integer] to always be on the left (first). -}

{-  The operator >&& "cons"es an integer twice to the beginning of a list.
    For example, 1 >&& [2, 3] = [1, 1, 2, 3]. It has type
    Integer -> [Integer] -> [Integer]. -}
-- (>&&) :: Integer -> [Integer] -> [Integer]
-- >&& infixr
-- Example: 0 >&& 1 >&& [2, 3] = 0 >&& (1 >&& [2, 3]) = [0, 0, 1, 1, 2, 3]
{-  Explanation: Function takes Integer and [Integer] as input and returns
    [Integer], so it can only take [Integer] as its second arg s.t. it must be
    right-associative for [Integer] to always be on the right (second). -}


-- 2. A curious operator
{-  Consider an operator +# that adds two integers and tells you how many 
    digits long their sum is. For example, 2 +# 800 = 3, since 802 is three
    digits long. It has type Integer -> Integer -> Integer. Comment on its
    associativity. -}
-- (+#) :: Integer -> Integer -> Integer
{-  The +# operator could be left- or right-associative because it takes 2
    Integers as input and returns an Integer such that the returned Integer
    could be either its first or second argument, so if we are going strictly
    off the type definition, it could be either left-associative or right-
    associative. However, if we want +# to work in a reasonable way, then it
    should be non-associative. The function is meant to take the sum and then
    count the number of digits, so if you wanted to chain applications
    together you may want to take the sum of all elements and then count the
    number of digits, but this isn't possible. It is also not reasonable to
    sum of the intermediate digit counts and subsequent numbers when
    successively chaining the function. Additionally, since addition is
    commutative, the result will be the same if you switch both the
    associativity and the order of the operands. Thus, there is no reasonable
    way to chain this operator such that it should be non-associative. -}
-- Example with left-associativity: 83 +# 9 +# 822 = (83 +# 9) +# 822 = 3
{-  Here, we sum 83 and 9 (=92), count digits (=2), then add that to 822
    (=824) and count digits again (3). This doesn't seem very reasonable. -}
-- Example with right-associativity: 83 +# 9 +# 822 = 83 +# (9 +# 822) = 2
{-  Here, we sum 9 and 822 (=831), count digits (=3), then add that to 83
    (=86) and count digits again. This doesn't seem very reasonable either. -}
