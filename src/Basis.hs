module Basis
( Lagrange(..)
, Legendre(..)
, Basis
, psi
, basisOrder
) where

-- Basis types
newtype Lagrange = Lagrange Int deriving (Show,Eq)
newtype Legendre = Legendre Int deriving (Show,Eq)

-- Type class for basis
class Basis b where
  psi        :: (Fractional a) => b -> a -> Int -> Int -> a
  basisOrder :: b -> Int

-- Instance for Lagrange basis
instance Basis Lagrange where

  -- Linear basis
  psi (Lagrange 1) xi i j | i == 0 && j == 0 = 1/2*(1 - xi)
                          | i == 1 && j == 0 = 1/2*(1 + xi)
                          | i == 0 && j == 1 = -1/2
                          | i == 1 && j == 1 = 1/2
                          | i >= 0 && j > 1 = 0
                          | otherwise = error "i must be 0 or 1."
  psi (Lagrange _) _ _ _ = error "Input error."

  -- Function for getting the order
  basisOrder (Lagrange n) = n
