{-# LANGUAGE ScopedTypeVariables #-}

-- Lab3ab.hs
module SparseMatrix where

-- PART C. SPARSE MATRIX MINI PROJECT

import qualified Data.Map as M
import qualified Data.Set as S


-- Custom data type for Sparse Matrixes
-- Row and cols are 1-indexed
-- Is polymorphic in a but a must be an instance of the Num type class
-- We cant constrain a to type class Num in the data definition and instead
-- will enforce this in all functions
data SparseMatrix a =
  SM { bounds     :: (Integer, Integer),  -- number of rows, columns
       rowIndices :: S.Set Integer,       -- row indices with nonzeros
       colIndices :: S.Set Integer,       -- column indices with nonzeros
       vals       :: (M.Map (Integer, Integer) a) }  -- values
  deriving (Eq, Show)
allZeroMatrix :: (Integer, Integer) -> SparseMatrix a
allZeroMatrix bs = 
    SM {bounds=bs, rowIndices=S.empty, colIndices=S.empty, vals=M.empty}


-- 0. Custom Helper Functions
-- Given bounds and an index, returns whether the index is in bounds
isBound :: (Integer, Integer) -> (Integer, Integer) -> Bool
isBound (nr, nc) (r, c) 
    | r > nr || c >= nc || r <= 0 || c <= 0 = False
isBound _ _ = True

-- Given map and key (or specifically values and an index), returns
-- value at key (index) in map (values) or 0 if key isn't in the map
mapGetOr0 :: Num a => (Integer, Integer) -> M.Map (Integer, Integer) a -> a
mapGetOr0 idx m = M.findWithDefault 0 idx m


-- 6. Accessors
-- Returns a value from a sparse matrix given the row and column
getSM :: Num a => SparseMatrix a -> (Integer, Integer) -> a
getSM (SM{bounds=(nr,nc)}) (r,c) | not (isBound (nr,nc) (r,c)) = 
    error "getSM: index is out of bounds of the matrix"
getSM (SM{vals=vs}) idx = mapGetOr0 idx vs

-- Returns the number of rows in a sparse matrix
rowsSM :: Num a => SparseMatrix a -> Integer
rowsSM (SM{bounds=(nr,_)}) = nr

-- Returns the number of columns in a sparse matrix
colsSM :: Num a => SparseMatrix a -> Integer
colsSM (SM{bounds=(_,nc)}) = nc


-- 1. sparseMatrix (Constructor from an OO perspective)
-- Creates a sparse matrix from list of index/value pairs and array bounds
sparseMatrix :: forall a . (Eq a, Num a) =>
  [((Integer, Integer), a)] -> (Integer, Integer) -> SparseMatrix a
sparseMatrix _ (nr, nc) | nr <= 0 || nc <= 0 = 
    error "sparseMatrix: bounds must be greater than 0 (1-indexed)"
sparseMatrix raw (nr, nc) = process raw S.empty S.empty M.empty
    where 
        process :: [((Integer, Integer), a)]
            -> S.Set Integer -> S.Set Integer -> M.Map (Integer, Integer) a
            -> SparseMatrix a
        process [] rs cs vs = 
            SM {bounds=(nr, nc), rowIndices=rs, colIndices=cs, vals=vs}
        process (((r,c), _): _) _ _ _ 
            | r > nr || c > nc || r <= 0 || c <= 0 =
            error "sparseMatrix: all pairs of indices for each value \
                \must be between 1 and the given number of rows or columns"
        process (((_,_), v): tl) rs cs vs | v == 0 = process tl rs cs vs
        process (((r,c), v): tl) rs cs vs = 
            process tl (S.insert r rs) (S.insert c cs) (M.insert (r,c) v vs)


-- 2. addSM
-- Adds two compatible sparse matrices, signalling error if dimensions don't match
addSM :: forall a . (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
addSM (SM{bounds=(nr1,nc1)}) (SM{bounds=(nr2,nc2)}) | nr1 /= nr2 || nc1 /= nc2 =
    error "addSM: matrices must have the same number of rows and columns" 
addSM (SM{bounds=bs, rowIndices=rIdxs1, colIndices=cIdxs1, vals=vs1})
    (SM{bounds=_, rowIndices=rIdxs2, colIndices=cIdxs2, vals=vs2}) =
        S.foldr rowIter (allZeroMatrix bs) (S.union rIdxs1 rIdxs2) 
            where
                addHelper :: (Integer, Integer) -> SparseMatrix a -> SparseMatrix a
                addHelper (r,c) sm@(SM{bounds=_, rowIndices=nrs, colIndices=ncs, vals=nvs}) =
                    let v = mapGetOr0 (r,c) vs1 + mapGetOr0 (r,c) vs2 in
                        if v == 0 then sm else 
                            SM {bounds=bs, rowIndices=S.insert r nrs, 
                                colIndices=S.insert c ncs, vals=M.insert (r,c) v nvs}
                rowIter :: Integer -> SparseMatrix a -> SparseMatrix a
                rowIter r mat =
                    S.foldr colIter mat (S.union cIdxs1 cIdxs2)
                        where 
                            colIter :: Integer -> SparseMatrix a -> SparseMatrix a
                            colIter c mat = addHelper (r,c) mat


-- 3. negateSM
-- Negates a sparse matrix
negateSM :: forall a . (Eq a, Num a) => SparseMatrix a -> SparseMatrix a
negateSM (SM {bounds=bs, rowIndices=rIdxs, colIndices=cIdxs, vals=vs}) =
    SM {bounds=bs, rowIndices=rIdxs, colIndices=cIdxs, vals=M.map (*(-1)) vs}  


-- 4. subSM
-- Subtracts two compatible sparse matrices, signalling error if dimensions don't match
subSM :: forall a . (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
subSM (SM{bounds=(nr1,nc1)}) (SM{bounds=(nr2,nc2)}) | nr1 /= nr2 || nc1 /= nc2 =
    error "subSM: matrices must have the same number of rows and columns" 
subSM mat1 mat2 = addSM mat1 (negateSM mat2)


-- 5. mulSM
-- Multiplies two compatible sparse matrices, signalling error if dimensions don't match
mulSM :: forall a . (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
mulSM (SM{bounds=(_,nc1)}) (SM{bounds=(nr2,_)}) | nc1 /= nr2 =
    error "mulSM: number of columns in first matrix \
        \must equal number of columns in second matrix"
mulSM (SM{bounds=bs, rowIndices=rIdxs1, colIndices=cIdxs1, vals=vs1})
    (SM{bounds=_, rowIndices=rIdxs2, colIndices=cIdxs2, vals=vs2}) =
        S.foldr rowIter (allZeroMatrix bs) rIdxs1 
            where
                {-  Vector multiplies r row of the first matrix with the c col of the second matrix
                    If the result is 0, nothing happens (returns same matrix). Otherwise, it stores
                    the non-zero result in (r, c) position and updates rowIndices and colIndices in
                    the returned sparseMatrix -}
                mulHelper :: (Integer, Integer) -> SparseMatrix a -> SparseMatrix a
                mulHelper (r,c) sm@(SM{bounds=_, rowIndices=nrs, colIndices=ncs, vals=nvs}) =
                    let v = S.foldr (\i sum -> sum + mapGetOr0 (r,i) vs1 * mapGetOr0 (i,c) vs2)
                            0 (S.intersection cIdxs1 rIdxs2) in
                        if v == 0 then sm else 
                            SM {bounds=bs, rowIndices=S.insert r nrs, 
                                colIndices=S.insert c ncs, vals=M.insert (r,c) v nvs}
                rowIter :: Integer -> SparseMatrix a -> SparseMatrix a
                rowIter r mat =
                    S.foldr colIter mat cIdxs2
                        where 
                            colIter :: Integer -> SparseMatrix a -> SparseMatrix a
                            colIter c mat = mulHelper (r,c) mat


-- Accessors for 6 are above


-- 7. Operators
(<|+|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|+|>) = addSM
(<|-|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|-|>) = subSM
(<|*|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|*|>) = mulSM
(<!>) :: Num a => SparseMatrix a -> (Integer, Integer) -> a
(<!>) = getSM


-- 8. Num instance of SparseMatrix?
{-  There are many reasons that it doesn't make sense to define the
    SparseMatrix datatype as an instance of the Num type class. The
    main ones that stick out to me are the fact that there is no equivalent
    notion of signum and fromInteger. The Num values within the sparseMatrix
    can have all different signs, so it is untenable to define a single sign
    for every SparseMatrix. Additionally, although a sparseMatrix of 0 can
    be considered equal to the Integer 0 from a 1-1 perspective, this is not
    true for every number (and in fact, most numbers). Think of the
    sparseMatrix equivalent of 6; making a reasonable 1-1 bijection in this
    way is also unintuitive and inconsistent. Due to this, we do not define
    SparseMatrix datatype as an instance of the Num type class. -}

{-
v1 = [((1,1),1), ((1,2),2), ((2,1),3), ((2,2),4)]
mat1 = sparseMatrix v1 (3,3)

v2 = [((1,1),3), ((2,1),5), ((3,1),2)]
mat2 = sparseMatrix v2 (3,3)
-}