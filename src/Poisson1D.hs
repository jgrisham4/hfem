{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Poisson1D
where

import qualified Basis
import           Data.List
import qualified Element
import           Mesh
import           Numeric.LinearAlgebra
import           Quadrature            (integrate)
import qualified ShapeFcns

-- Synonym for numeric/fractional constraints
type FrElNuFi a = (Fractional a,Element a,Numeric a,Field a)

-- Element stiffness integrand
stiffnessIntegrand :: (Element.Element e,ShapeFcns.ShapeFcn s,Basis.Basis b,FrElNuFi a) => e a -> s b -> [a] -> Matrix a
stiffnessIntegrand elem shpFcn xi = fromLists [[]]

-- Element mass integrand
massIntegrand :: (Element.Element e,ShapeFcns.ShapeFcn s,Basis.Basis b,FrElNuFi a) => e a -> s b -> [a] -> Matrix a
massIntegrand elem shpFcn xi = fromLists [[]]

-- Element mass and stiffness matrices
-- Must multiply by the determinant of the Jacobian here!!!
elemMatrices :: (Element.Element e,ShapeFcns.ShapeFcn s,Basis.Basis b,FrElNuFi a) => e a -> s b -> Int -> (Matrix a,Matrix a)
elemMatrices elem shpFcn ngpts = (stiffnessMat, massMat)
  where
    stiffnessMat = integrate 1 ngpts (stiffnessIntegrand elem shpFcn)
    massMat      = integrate 1 ngpts (massIntegrand elem shpFcn)

-- Function which assembles the global stiffness and mass matrices
assembleGlobalMatrices :: (Element.Element e, ShapeFcns.ShapeFcn s,Basis.Basis b, FrElNuFi a) => Mesh e a -> s b -> Int -> a -> [[((Int,Int),a)]]
assembleGlobalMatrices grid shpFcn ngpts advSpd = [globalK, globalM, globalF]
  where
    globalK = []
    globalM = []
    globalF = []

-- Function for applying boundary conditions
applyBCs _ = []
