{-# LANGUAGE ConstraintKinds #-}

module Advection1D
where

import qualified Basis
import           Data.List
import qualified Element
import           Flux
import           Mesh
import           Numeric.LinearAlgebra.HMatrix
import           Quadrature                    (integrate)
import qualified ShapeFcns

-- Synonym for numeric/fractional constraints
type FrElNuFi a = (Fractional a,Element a,Numeric a,Field a)

-- Element stiffness integrand
stiffnessIntegrand :: (Element.Element e,ShapeFcns.ShapeFcn s,Basis.Basis b,FrElNuFi a) => e a -> s b -> [a] -> Matrix a
--stiffnessIntegrand elem shpFcn xi = fromLists [[
--  ShapeFcns.n shpFcn xi i [0] * atIndex (Element.dndx elem shpFcn xi j) 0 * Element.computeJacobianDet elem shpFcn xi
--  | i <- idxRange] | j <- idxRange]
stiffnessIntegrand elem shpFcn xi = fromLists [[
  (atIndex (Element.dndx elem shpFcn xi i) 0 * atIndex (Element.dndx elem shpFcn xi j) 0 + ShapeFcns.n shpFcn xi i [0] * ShapeFcns.n shpFcn xi j [0]) * Element.computeJacobianDet elem shpFcn xi
  | i <- idxRange] | j <- idxRange]
  where
    idxRange = [0..(Element.getNumNodes elem - 1)]

-- Element mass integrand
massIntegrand :: (Element.Element e,ShapeFcns.ShapeFcn s,Basis.Basis b,FrElNuFi a) => e a -> s b -> [a] -> Matrix a
massIntegrand elem shpFcn xi = fromLists [[
  ShapeFcns.n shpFcn xi i [0] * ShapeFcns.n shpFcn xi j [0] * Element.computeJacobianDet elem shpFcn xi
  | i <- idxRange] | j <- idxRange]
  where
    idxRange = [0..(ShapeFcns.getShapeFcnOrder shpFcn)]

-- Element mass and stiffness matrices
-- Must multiply by the determinant of the Jacobian here!!!
elemMatrices :: (Element.Element e,ShapeFcns.ShapeFcn s,Basis.Basis b,FrElNuFi a) => e a -> s b -> Int -> (Matrix a,Matrix a)
elemMatrices elem shpFcn ngpts = (stiffnessMat, massMat)
  where
    stiffnessMat = integrate 1 ngpts (stiffnessIntegrand elem shpFcn)
    massMat      = integrate 1 ngpts (massIntegrand elem shpFcn)

-- Function which computes the dimensions of the global matrices
globalMatrixDimension :: Int -> Int
globalMatrixDimension numNodes = 2 * numNodes - 2

-- Function which takes a local element matrix along with an element and transforms it to global
localMatToGlobal :: (Element.Element e,FrElNuFi a) => e a -> Matrix a -> [((Int, Int), a)]
localMatToGlobal elem elemMat = concat [[((Mesh.globalNodeNum en i, Mesh.globalNodeNum en j), atIndex elemMat (i,j) ) | i <- [0..dim-1]] | j <- [0..dim-1]]
  where
    en = Element.getElementNumber elem
    dim = (fst . size) elemMat

-- Function which assembles global stiffness and mass matrices
-- This function takes a mesh and returns a tuple which corresponds to the
-- global stiffness and global mass matrices, respectively.
-- THIS ONLY WORKS WITH THE DISCONTINUOUS MESH...  IT DOES NOT SUM DUPLICATE ENTRIES.
assembleGlobalMatrices :: (Element.Element e, ShapeFcns.ShapeFcn s,Basis.Basis b, FrElNuFi a) => Mesh e a -> s b -> Int -> ([((Int,Int),a)],[((Int,Int),a)])
assembleGlobalMatrices grid shpFcn ngpts = (globalK, globalM)
  where
    elems = Mesh.getMeshElements grid
    elemMats = map (\elem -> elemMatrices elem shpFcn ngpts) elems
    elemK = map fst elemMats
    elemM = map snd elemMats
    globalK = concat (zipWith localMatToGlobal elems elemK)
    globalM = concat (zipWith localMatToGlobal elems elemM)
