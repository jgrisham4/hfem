module ShapeFcns
(
  TensorProduct(..)
, NaturalSimplex(..)
, shpFcnBasisMap
, getBasis
, getShapeFcnOrder
, n
, dndXi
) where

import Basis

-- Need to define mapping from a to i or i,j or i,j,k
-- ONLY WORKS FOR LINEAR BASIS
--foldr (++) [] $ replicate 2 [1,0]
shpFcnBasisMap :: Int -> Int -> [Int]
shpFcnBasisMap nodeNum 1 = [concat (replicate 2 [0, 1, 1, 0]) !! nodeNum]
shpFcnBasisMap nodeNum 2 = shpFcnBasisMap nodeNum 1 ++ [concat (replicate 2 [0, 0, 1, 1]) !! nodeNum]
shpFcnBasisMap nodeNum 3 = shpFcnBasisMap nodeNum 2 ++ [(replicate 4 0 ++ replicate 4 1) !! nodeNum]
shpFcnBasisMap _ _ = error "Only defined for 0<=nodeNum<=7 and up to 3D."

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
  n :: (BasisType a,Fractional c) => f a -> [c] -> Int -> c

  -- Returns [dn_a/dxi, dn_a/deta, ... ]
  dndXi :: (BasisType a,Fractional c) => f a -> [c] -> Int -> [c]

-- ShapeFcnType instance for tensor product shape functions
instance ShapeFcnType TensorProduct where

  getShapeFcnOrder (TensorProduct basis _) = basisOrder basis

  getBasis (TensorProduct basis _) = basis

  n (TensorProduct basis 1  ) coords a = psi basis (last coords) (last (shpFcnBasisMap a 1))
  n (TensorProduct basis dim) coords a = psi basis (last coords) (last (shpFcnBasisMap a dim)) * n (TensorProduct basis (dim-1)) (take (dim-1) coords) a

  dndXi (TensorProduct basis 1) coords a = [dpsidxi basis (last coords) a 1]
  dndXi (TensorProduct basis 2) coords a = [dndxi, dndeta]
    where
      dndxi  = dpsidxi basis (head coords) (head $ shpFcnBasisMap a 2) 1 * psi basis (last coords) (last $ shpFcnBasisMap a 2)
      dndeta = psi basis (head coords) (head $ shpFcnBasisMap a 2) * dpsidxi basis (last coords) (last $ shpFcnBasisMap a 2) 1
  dndXi _ _ _ = error "dndXi only defined for 1d and 2d for now."
