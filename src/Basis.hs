module Basis
( Lagrange(..)
, Legendre(..)
, psi
, dpsidxi
, order
) where

-- Basis types
newtype Lagrange = Lagrange Int deriving (Show,Eq)
newtype Legendre = Legendre Int deriving (Show,Eq)

-- Type class for basis
class BasisType b where
  psi     :: (Floating a) => b -> a -> Int -> a
  dpsidxi :: (Floating a) => b -> a -> Int -> Int -> a
  order   :: b -> Int

-- Instance for Lagrange basis
instance BasisType Lagrange where

  -- Linear basis
  psi (Lagrange 1) xi i | i == 0 = 1/2*(1 - xi)
                        | i == 1 = 1/2*(1 + xi)
                        | otherwise = error "i must be 0 or 1."
  psi (Lagrange _) _ _ = error "Input error."

  -- Linear basis derivative
  dpsidxi (Lagrange 1) xi i n | i == 0 = -1/2
                              | i == 1 = 1/2
                              | otherwise = error "i must be 0 or 1."

  -- Function for getting the order
  order (Lagrange n) = n
