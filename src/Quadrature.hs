{-# LANGUAGE ConstraintKinds #-}

module Quadrature
( getGaussPoints
, integrate
) where

import qualified Numeric.LinearAlgebra.HMatrix as HMat

type FrElNuFi a = (Fractional a,HMat.Element a,HMat.Numeric a,HMat.Field a)

-- Function for getting Gauss points for integration
-- Given the number of points it returns a list of the points and the weights
--getGaussPoints :: Fractional a => Int -> ([a], [a])
getGaussPoints :: Int -> ([Double], [Double])
getGaussPoints 1 = ([0.0], [2.0])
getGaussPoints 2 = ([-0.577350269189626, 0.577350269189626], [1.0, 1.0])
getGaussPoints 3 = ([-0.774596669241483, 0.0, 0.774596669241483], [0.555555555555555, 0.888888888888889, 0.555555555555555])
getGaussPoints _ = error "Gauss points only up to 3 point quadrature available."

-- Function for integrating some integrand over an element
-- Usage: integrate dim ngpts integrand
--integrate :: FrElNuFi a => Int -> Int -> (a -> HMat.Matrix a) -> HMat.Matrix a
integrate :: Int -> Int -> (Double -> HMat.Matrix Double) -> HMat.Matrix Double
integrate 1 ngpts integrand = sum (zipWith HMat.scale wts (map integrand xi))
  where
    gpts = getGaussPoints ngpts
    xi = fst gpts
    wts = snd gpts
integrate _ _ _ = error "Integration only works in 1d for now."

