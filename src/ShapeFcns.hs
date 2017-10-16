module ShapeFcns
(
  TensorProduct(..)
, NaturalSimplex(..)
, shpFcnBasisMap
, getBasis
, getShapeFcnOrder
, n
--, dndXi
) where

{-
IMPORTANT NOTE: The current implementation of the recursive shape function is inefficient because
a list in haskell is singly-linked.  Taking an element from the end is computationally expensive.
The problem is tricky though because the recursion is from the n-th dimension down to the 1st
dimension.  The problem is in the map which defines which basis function to use given a shape
function number.  This is defined in the function shpFcnBasisMap.  Need to think about how to
mitigate this problem.  See the IrcLog in refs for discussion.
-}

import Basis

-- Need to define mapping from a to i or i,j or i,j,k
-- ONLY WORKS FOR LINEAR BASIS
shpFcnBasisMap :: Int -> Int -> [Int]
shpFcnBasisMap nodeNum 1 = [concat (replicate 2 [0, 1, 1, 0]) !! nodeNum]
shpFcnBasisMap nodeNum 2 = shpFcnBasisMap nodeNum 1 ++ [concat (replicate 2 [0, 0, 1, 1]) !! nodeNum]
shpFcnBasisMap nodeNum 3 = shpFcnBasisMap nodeNum 2 ++ [(replicate 4 0 ++ replicate 4 1) !! nodeNum]
shpFcnBasisMap _ _ = error "Only defined for 0<=nodeNum<=7 and up to 3D."

-- Map which flips first dimension to last.  This essentially takes the transpose of the shpFcnBasisMap

-- Shape function types (dependent upon basis used)
-- b represents the basis, and the integer represents the dimension
data TensorProduct  b = TensorProduct  b Int deriving (Show,Eq)
data NaturalSimplex b = NaturalSimplex b Int deriving (Show,Eq)

-- Type class for shape functions.  This defines a set of functions which apply
-- to all shape functions.
class ShapeFcnType f where

  -- Returns the order of the shape function
  getShapeFcnOrder :: BasisType a => f a -> Int

  -- Returns the basis which is used to build up the tensor product shape function
  getBasis :: BasisType a => f a -> a

  -- Returns the value of the i-th shape function at the given (xi,eta,...) coords
  -- The last argument is number of derivatives. [1,0,0] = dn/dxi
  n :: (BasisType a,Fractional c) => f a -> [c] -> Int -> [Int] -> c

  -- Returns [dn_a/dxi, dn_a/deta, ... ]
  --dndXi :: (BasisType a,Fractional c) => f a -> [c] -> Int -> [c]

-- ShapeFcnType instance for tensor product shape functions
instance ShapeFcnType TensorProduct where

  getShapeFcnOrder (TensorProduct basis _) = basisOrder basis

  getBasis (TensorProduct basis _) = basis

  n (TensorProduct basis 1  ) coords a deriv = psi basis (last coords) (last (shpFcnBasisMap a 1)) (last deriv)
  n (TensorProduct basis dim) coords a deriv = psi basis (last coords) (last (shpFcnBasisMap a dim)) (last deriv) * n (TensorProduct basis (dim-1)) (take (dim-1) coords) a (take (dim-1) deriv)

  --dndXi (TensorProduct basis 1) coords a = [dpsidxi basis (last coords) a 1]
  --dndXi (TensorProduct basis 2) coords a = [dndxi, dndeta]
  --  where
  --    dndxi  = dpsidxi basis (head coords) (head $ shpFcnBasisMap a 2) 1 * psi basis (last coords) (last $ shpFcnBasisMap a 2)
  --    dndeta = psi basis (head coords) (head $ shpFcnBasisMap a 2) * dpsidxi basis (last coords) (last $ shpFcnBasisMap a 2) 1
  --dndXi _ _ _ = error "dndXi only defined for 1d and 2d for now."
