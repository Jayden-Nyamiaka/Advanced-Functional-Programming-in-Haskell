{-# LANGUAGE ScopedTypeVariables #-}

-- Scratch.hs
module Scratch where
-- Used for Sratch Work Code


-- import Prelude
import qualified Data.Map as M
import qualified Data.Set as S

data Expl = Pair {first::Integer, second::Integer}
    deriving (Eq, Show)
twicePair :: Integer -> Expl
twicePair x = Pair {first=f, second=s} 
    where 
        funcEx :: Integer -> (Integer, Integer)
        funcEx n = (n, 2*n)
        (f, s) = funcEx x
            