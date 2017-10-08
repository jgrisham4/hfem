module ShapeFcns
(
  TensorProduct(..)
, NaturalSimplex(..)
) where

import Basis

-- Shape function types (dependent upon basis used)
-- b represents the basis
newtype TensorProduct  b = TensorProduct  b deriving (Show,Eq)
newtype NaturalSimplex b = NaturalSimplex b deriving (Show,Eq)

class ShapeFcn f where
  getBasis :: (BasisType bt) => f -> bt
--  n     :: (Floating a) => f -> [a] -> Int -> a
--  dndXi :: (Floating a) => f -> [a] -> Int -> [a]

-- Instance for Tensor product using Lagrange basis
--instance ShapeFcn TensorProduct where
--  getBasis (TensorProduct b) = b

  -- Linear Lagrange basis
 -- n (TensorProduct
