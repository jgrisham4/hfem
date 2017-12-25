{-# LANGUAGE ConstraintKinds #-}

module Projection
( projectL2
)
where

import           Basis
import           Element
import           Node
import qualified Numeric.LinearAlgebra as LinAl
import           Quadrature
import           ShapeFcns

-- Synonym for numeric/fractional constraints
type FrElNuFi a = (Fractional a,LinAl.Element a,LinAl.Numeric a,LinAl.Field a)

-- Element mass integrand
elemMassIntegrand :: (Element e,ShapeFcn s,Basis b,FrElNuFi a) => e a -> s b -> [a] -> LinAl.Matrix a
elemMassIntegrand elem shpFcn xi = LinAl.fromLists [[
  ShapeFcns.n shpFcn xi i [0] * ShapeFcns.n shpFcn xi j [0] * Element.computeJacobianDet elem shpFcn xi
  | i <- idxRange] | j <- idxRange]
  where
    idxRange = [0..(ShapeFcns.getShapeFcnOrder shpFcn)]

-- This function computes the element load integrand when given a function
-- to integrate over an element.
elemLoadIntegrand :: (Element e,ShapeFcn s,Basis b,FrElNuFi a) => e a -> s b -> ([a] -> a) -> [a] -> LinAl.Matrix a
elemLoadIntegrand elem shpFcn f xi = LinAl.scale (computeJacobianDet elem shpFcn xi) (LinAl.fromLists $ map (:[]) [f physCoords * n shpFcn xi i [0] | i <- [0..(getShapeFcnOrder shpFcn)]])
  where
    physCoords = [sum $ zipWith (*) (map (head . nodeCoordinates) (getElementNodes elem)) (map (\j -> n shpFcn xi j [0]) [0..(getShapeFcnOrder shpFcn)])]

-- Function for projecting a function onto a basis over a single element.
projectL2 :: (Element e,ShapeFcn s,Basis b,FrElNuFi a) => e a -> s b -> Int -> ([a] -> a) -> Maybe (LinAl.Matrix a)
projectL2 elem shpFcn ngpts f = LinAl.linearSolve (integrate 1 ngpts (elemMassIntegrand elem shpFcn)) (integrate 1 ngpts (elemLoadIntegrand elem shpFcn f))
