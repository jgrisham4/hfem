{-# LANGUAGE ConstraintKinds #-}

module Quadrature
( getGaussPoints
, numGaussPoints
, integrate
, integrateFunc
) where

import qualified Numeric.LinearAlgebra.HMatrix as HMat

type FrElNuFi a = (Fractional a,HMat.Element a,HMat.Numeric a,HMat.Field a)
type FlElNuFi a = (Floating a,HMat.Element a,HMat.Numeric a,HMat.Field a)

-- Function for computing the required number of Gauss points for a polynomial
numGaussPoints :: Integral a => a -> a
numGaussPoints order = ceiling (0.5 * (fromIntegral order + 1.0))

-- Function for getting Gauss points for integration
-- Given the number of points it returns a list of the points and the weights
getGaussPoints :: Floating a => Int -> ([a], [a])
getGaussPoints 1 = ([0.0], [2.0])
getGaussPoints 2 = ([-1.0/sqrt 3.0, 1.0/sqrt 3.0], [1.0, 1.0])
getGaussPoints 3 = ([-sqrt (3.0/5.0), 0.0, sqrt (3.0/5.0)], [5.0/9.0, 8.0/9.0, 5.0/9.0])
getGaussPoints 4 = ([-0.861136311594053,-0.339981043584856,0.339981043584856,0.861136311594053],[0.347854845137454,0.652145154862546,0.652145154862546,0.347854845137454])
getGaussPoints _ = error "Gauss points only up to 4 point quadrature available."

-- Function for integrating some integrand over an element
-- Usage: integrate dim ngpts integrand
integrate :: FrElNuFi a => Int -> Int -> ([a] -> HMat.Matrix a) -> HMat.Matrix a
integrate 1 ngpts integrand = foldl1 HMat.add $ zipWith HMat.scale wts (map (\x -> integrand [x]) xi)
  where
    gpts = getGaussPoints ngpts
    xi = fst gpts
    wts = snd gpts
integrate _ _ _ = error "Integration only works in 1d for now."

-- Function for integrating a function from -1 to 1
integrateFunc :: Floating a => Int -> (a -> a) -> a
integrateFunc ngpts func = sum $ zipWith (*) wts (map func pts)
  where
    gpts = getGaussPoints ngpts
    pts = fst gpts
    wts = snd gpts
