{-# LANGUAGE ConstraintKinds #-}

module Quadrature
( getGaussPoints
, integrate
) where

import qualified Numeric.LinearAlgebra.HMatrix as HMat

type FrElNuFi a = (Fractional a,HMat.Element a,HMat.Numeric a,HMat.Field a)
type FlElNuFi a = (Floating a,HMat.Element a,HMat.Numeric a,HMat.Field a)

-- Function for getting Gauss points for integration
-- Given the number of points it returns a list of the points and the weights
getGaussPoints :: Floating a => Int -> ([a], [a])
getGaussPoints 1 = ([0.0], [2.0])
getGaussPoints 2 = ([-1.0/sqrt 3.0, 1.0/sqrt 3.0], [1.0, 1.0])
getGaussPoints 3 = ([-sqrt (3.0/5.0), 0.0, sqrt (3.0/5.0)], [5.0/9.0, 8.0/9.0, 5.0/9.0])
getGaussPoints _ = error "Gauss points only up to 3 point quadrature available."

-- Function for integrating some integrand over an element
-- Usage: integrate dim ngpts integrand
integrate :: FrElNuFi a => Int -> Int -> ([a] -> HMat.Matrix a) -> HMat.Matrix a
integrate 1 ngpts integrand = foldl1 HMat.add $ zipWith HMat.scale wts (map integrand [xi])
  where
    gpts = getGaussPoints ngpts
    xi = fst gpts
    wts = snd gpts
integrate _ _ _ = error "Integration only works in 1d for now."

