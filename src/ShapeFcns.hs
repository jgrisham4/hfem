module ShapeFcns
(
  TensorProduct(..)
, NaturalSimplex(..)
, getBasis
, n
) where

import Basis

-- Need to define mapping from i to i,j and i,j,k
-- TODO Generalize beyond linear basis.
shpFcnBasisMap :: Int -> Int -> [Int]
shpFcnBasisMap nodeNum 1 = [nodeNum]
shpFcnBasisMap nodeNum 2 | nodeNum == 0 = [0, 0]
                         | nodeNum == 1 = [1, 0]
                         | nodeNum == 2 = [1, 1]
                         | nodeNum == 3 = [0, 1]

-- Shape function types (dependent upon basis used)
-- b represents the basis
newtype TensorProduct  b = TensorProduct  b deriving (Show,Eq)
newtype NaturalSimplex b = NaturalSimplex b deriving (Show,Eq)

class ShapeFcnType f where
  getBasis :: BasisType a => f a -> a
  n        :: (BasisType a,Floating c) => f a -> [c] -> Int -> c
--  dndXi :: (Floating a) => f -> [a] -> Int -> [a]

instance ShapeFcnType TensorProduct where
  getBasis (TensorProduct a) = a
  n (TensorProduct basis) coords i = psi basis (head coords) iInd * psi basis (coords !! 1) jInd
    where
      basisIndices = shpFcnBasisMap i 2
      iInd = head basisIndices
      jInd = last basisIndices
