module Basis
( Lagrange(..)
, Legendre(..)
, BasisType
, psi
, dpsidxi
, basisOrder
) where

-- Basis types
newtype Lagrange = Lagrange Int deriving (Show,Eq)
newtype Legendre = Legendre Int deriving (Show,Eq)

-- Type class for basis
class BasisType b where
  psi        :: (Fractional a) => b -> a -> Int -> a
  dpsidxi    :: (Fractional a) => b -> a -> Int -> Int -> a
  basisOrder :: b -> Int

-- Instance for Lagrange basis
-- TODO change psi so that it takes i and j where i corresponds to the node number and j cooresponds to the order of the derivative taken.
instance BasisType Lagrange where

  -- Linear basis
  psi (Lagrange 1) xi i | i == 0 = 1/2*(1 - xi)
                        | i == 1 = 1/2*(1 + xi)
                        | otherwise = error "i must be 0 or 1."
  psi (Lagrange _) _ _ = error "Input error."

  -- Linear basis derivative (all derivatives higher than 1st are zero)
  dpsidxi (Lagrange 1) xi i 1 | i == 0 = -1/2
                              | i == 1 = 1/2
                              | otherwise = error "i must be 0 or 1."
  dpsidxi (Lagrange 1) xi i _ | i == 0 || i == 1 = 0
                              | otherwise = error "i must be 0 or 1."

  -- Function for getting the order
  basisOrder (Lagrange n) = n
