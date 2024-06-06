-- State.hs
module State where

import Control.Monad
import Control.Monad.State
import Data.IORef

--
-- Part A. IORefs vs State Monads
--

-- Part A While Loop Helper Functions
-- While loop in the IO monad.
whileIO :: IO Bool -> IO () -> IO () 
whileIO test block = 
  do b <- test 
     when b (do block
                whileIO test block)

-- While loop in the state monad.
whileState :: (s -> Bool) -> State s () -> State s ()
whileState test body = 
  do s0 <- get
     when (test s0)
          (do body
              whileState test body)


-- A.1. Computes the nth factorial using the IO monad.
factIO :: Integer -> IO Integer
factIO n = do 
  i <- newIORef n
  res <- newIORef 1
  whileIO
    (do
      i' <- readIORef i
      return $ i' > 0) 
    (do
      i' <- readIORef i
      res' <- readIORef res
      writeIORef res (res' * i')
      writeIORef i (i' - 1))
  readIORef res


-- A.2. Computes the nth factorial using the state monad.
factState :: Integer -> Integer
factState n = evalState factSt (n, 1)
factSt :: State (Integer, Integer) Integer
factSt = do 
  whileState (\(i, _) -> i > 0) 
    (do 
      (i, res) <- get
      put (i - 1, res * i))
  (_, res) <- get
  return res


-- A.3. Computes the nth fibonacci number using the IO monad.
fibIO :: Integer -> IO Integer
fibIO n = do 
  i <- newIORef n
  fs <- newIORef 0
  fb <- newIORef 1
  whileIO
    (do
      i' <- readIORef i
      return $ i' > 0) 
    (do
      i' <- readIORef i
      fs' <- readIORef fs
      fb' <- readIORef fb
      writeIORef fb (fb' + fs')
      writeIORef fs fb'
      writeIORef i (i' - 1))
  readIORef fs


-- A.4. Computes the nth fibonacci number using the state monad.
fibState :: Integer -> Integer
fibState n = evalState fibSt (0, 1, n)
fibSt :: State (Integer, Integer, Integer) Integer
fibSt = do 
  whileState (\(_, _, i) -> i > 0) 
    (do 
      (fs, fb, i) <- get
      put (fb, fb + fs, i - 1))
  (fs, _, _) <- get
  return fs



--
-- Part B. Deriving the Reader Monad
--

{- 
  We'll derive >>= and return for the Reader monad from first principles. 
  Note that the >>= operator has to be defined to make monadic function
  composition work correctly, and the return operator is the monadic
  identity function. 

  The first monadic principles of the Reader monad are as follows...
  Conceptual Computation: 
    a -------[read from a stored state]------> b
  Datatype Definition:
    data Reader r b = Reader (r -> b)
  Non-Monadic Type Signature: 
    (a, r) -> b    ==    a -> r -> b    =    a -> (r -> b)
  Monad Type Signature:
    a -> Reader r b with (Reader r) as Monad Instance

    Let's start with trying to derive the >>= operator. Since it's made for
    composition, let's define f, g, and h as functions for composition.
      f :: a -> Reader r b
      g :: b -> Reader r c
      h :: a -> Reader r c
    If we compose f and g, the result should be h.

    From above, we know the non-monadic type signatures are as follows.
      f' :: a -> (r -> b)
      g' :: b -> (r -> c)
      h' :: a -> (r -> c)

    First, let's derive a few things.
    
    Define h' in terms of f' and g'
      h' :: a -> (r -> c)
      h' (x, rd) = 
        let y = f' (x, rd)
          z = g' (y, rd')
        in z

    Define complete curried versions with the following definitions.
      f'' :: a -> r -> b
      g'' :: b -> r -> c
      f'' x = \rd -> f' (x, rd)
      g'' y = \rd -> g' (y, rd)

    Wrapping right hand sides with a Reader constructor yields monadic versions.
      f x = Reader (\rd -> f' (x, rd))
      g y = Reader (\rd -> g' (y, rd))
      h x = Reader (\rd -> h' (x, rd))

    From here, we can derive the >>= operator.
      h = f >=> g
      h x = f x >>= g
      f x >>= g = h x
      f x >>= g = Reader (\rd -> h' (x, rd))
    [Substitute h' with definition above]
      f x >>= g = Reader (\rd -> 
        let y = f' (x, rd)
          z = g' (y, rd)
        in z)
    [Simplify redundant z let statement]
      f x >>= g = Reader (\rd -> 
        let y = f' (x, rd)
        in g' (y, rd))
    [Unpack f x using f x = Reader (\rd -> f' (x, rd))]
      f x >>= g = Reader (\rd -> 
        let (Reader ff) = f x
          y = ff rd
        in g' (y, rd))
    [Unpack g x in the same way]
      f x >>= g = Reader (\rd -> 
        let (Reader ff) = f x
          y = ff rd
          (Reader gg) = g y
        in gg rd)
    [Substitute mx for f x as the monadic value]
      mx >>= g = Reader (\rd -> 
        let (Reader ff) = mx
          y = ff rd
          (Reader gg) = g y
        in gg rd)
    A trivial name change evinces this is equivalent to the given definition.
      mx >>= f = Reader (\r ->
        let (Reader g) = mx
          x = g r
          (Reader h) = f x
        in h r)
    Thus, we've derived the >>= operator.

    Now, we move on to derive the return operator. Since Reader instances are
    functions, we can do this by considering the identity function that has
    the type signature: a -> Reader r b. 

    Trivially, this means we should have a function that takes a type a along
    with the reader type r and simply returns the reader. 

    For the non-monadic type signature, we have (a, r) -> b, so the identity
    function in this from would simply be 
      id_func (x, rd) = x
      id_func' x = (\rd -> x).

    Then, for the monadic type signature, we can similarly write this as
      id_func_monad x = Reader (\rd -> x)
      return x = Reader (\rd -> x)
    
    This conforms to the monadic type signature and is the monadic identity
    function such that we have successfully derived the return operator.
-}
