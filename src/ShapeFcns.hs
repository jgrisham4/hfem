module ShapeFcns
( TensorProduct(..)
, NaturalSimplex(..)
, ShapeFcn
, shpFcnBasisMap
, getBasis
, getShapeFcnOrder
, getDimension
, n
, dndXi
) where

{-
IMPORTANT NOTE: The current implementation of the recursive shape function is inefficient because
a list in haskell is singly-linked.  Taking an element from the end is computationally expensive.
The problem is tricky though because the recursion is from the n-th dimension down to the 1st
dimension.  The problem is in the map which defines which basis function to use given a shape
function number.  This is defined in the function shpFcnBasisMap.  Need to think about how to
mitigate this problem.  See the IrcLog in refs for discussion.
-}

import           Basis
import           Control.Monad

-- Need to define mapping from a to i or i,j or i,j,k (N_a -> N_ijk)
-- ONLY WORKS FOR LINEAR BASIS
shpFcnBasisMap :: Int -> Int -> Int -> [Int]
shpFcnBasisMap 1 _ nodeNum = [concat (replicate 2 [0, 1, 1, 0]) !! nodeNum]
shpFcnBasisMap 2 _ nodeNum = shpFcnBasisMap 1 1 nodeNum ++ [concat (replicate 2 [0, 0, 1, 1]) !! nodeNum]
shpFcnBasisMap 3 _ nodeNum = shpFcnBasisMap 2 1 nodeNum ++ [(replicate 4 0 ++ replicate 4 1) !! nodeNum]
shpFcnBasisMap _ _ _ = error "Only defined for 0<=nodeNum<=7 and up to 3D."

-- Alternate mapping.  More generic, but incomplete.
--shpFcnBasisMap' :: Int -> Int -> Int -> [Int]
--shpFcnBasisMap' dim order nodeNum = map reverse (replicateM dim [0..order]) !! nodeNum

-- Shape function types (dependent upon basis used)
-- b represents the basis, and the integer represents the dimension
data TensorProduct  b = TensorProduct  b Int deriving (Show,Eq)
data NaturalSimplex b = NaturalSimplex b Int deriving (Show,Eq)

-- Type class for shape functions.  This defines a set of functions which apply
-- to all shape functions.
class ShapeFcn f where

  getShapeFcnOrder :: Basis b => f b -> Int
  getDimension     :: Basis b => f b -> Int
  getBasis         :: Basis b => f b -> b

  -- Returns the value of the i-th shape function at the given (xi,eta,...) coords
  -- The last argument is number of derivatives. [1,0,0] = dn/dxi, [0,0,1] = dn/dzeta
  n :: (Basis b,Fractional c) => f b -> [c] -> Int -> [Int] -> c

  -- Returns [dn_a/dxi, dn_a/deta, ... ]
  dndXi :: (Basis b,Fractional c) => f b -> [c] -> Int -> [c]

-- ShapeFcn instance for tensor product shape functions
instance ShapeFcn TensorProduct where

  getShapeFcnOrder (TensorProduct basis _) = basisOrder basis
  getDimension (TensorProduct _ dim) = dim
  getBasis (TensorProduct basis _) = basis

  n (TensorProduct basis 1) coords a deriv = psi basis (last coords) (last (shpFcnBasisMap 1 (basisOrder basis) a)) (last deriv)
  n (TensorProduct basis dim) coords a deriv =
    psi basis (last coords) (last (shpFcnBasisMap dim (basisOrder basis) a)) (last deriv) *
    n (TensorProduct basis (dim-1)) (take (dim-1) coords) a (take (dim-1) deriv)

  dndXi (TensorProduct basis 1) coords a = [n (TensorProduct basis 1) coords a [1]]
  dndXi (TensorProduct basis 2) coords a =
    [n (TensorProduct basis 2) coords a [1,0],
     n (TensorProduct basis 2) coords a [0,1]]
  dndXi (TensorProduct basis 3) coords a =
    [n (TensorProduct basis 3) coords a [1,0,0],
     n (TensorProduct basis 3) coords a [0,1,0],
     n (TensorProduct basis 3) coords a [0,0,1]]
