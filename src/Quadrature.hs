module Quadrature (getGaussPoints) where

-- Function for getting Gauss points for integration
-- Given the number of points it returns a list of the points and the weights
getGaussPoints :: (Fractional a) => Int -> ([a], [a])
getGaussPoints 1 = ([0.0], [2.0])
getGaussPoints 2 = ([-0.577350269189626, 0.577350269189626], [1.0, 1.0])
getGaussPoints 3 = ([-0.774596669241483, 0.0, 0.774596669241483], [0.555555555555555, 0.888888888888889, 0.555555555555555])
getGaussPoints _ = error "Gauss points only up to 3 point quadrature available."
