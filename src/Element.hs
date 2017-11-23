module Element
( Element
, StructElem(..)
, getElementNodes
, getConnectivity
, getElementNumber
, computeJacobian
, computeJacobianDet
) where

import qualified Numeric.LinearAlgebra         as L
import qualified Numeric.LinearAlgebra.HMatrix as HMat

import           Basis
import           Node
import           Quadrature
import           ShapeFcns

-- Utility functions
allCombinations :: [Int] -> [[Int]]
allCombinations x = mapM (const x) [1..(length x)]
constMat :: (Num a,L.Element a) => Int -> a -> L.Matrix a
constMat sz val = L.fromLists [[val | i <- [0..(sz-1)]] | j <- [0..(sz-1)]]

-- Element types
data StructElem a = StructElem [Node a] Int deriving (Show,Eq)
data Tri a        = Tri        [Node a] Int deriving (Show,Eq)

-- Only going to implement quads for now

class Element e where
  getElementNodes    :: (Fractional a) => e a -> [Node a]
  getNumNodes        :: (Fractional a) => e a -> Int
  getConnectivity    :: (Fractional a) => e a -> [Int]
  getElementNumber   :: (Fractional a) => e a -> Int
  getElementNumbor   :: (Fractional a) => e a -> Int
  computeJacobian    :: (Basis b,ShapeFcn s,Fractional a,L.Element a,L.Numeric a) => e a -> s b -> [a] -> L.Matrix a
  computeJacobianDet :: (Basis b,ShapeFcn s,Fractional a,L.Element a,L.Numeric a,L.Field a) => e a -> s b -> [a] -> a
  dndx               :: (Basis b,ShapeFcn s,Fractional a,L.Element a,L.Numeric a,L.Field a) => e a -> s b -> [a] -> Int -> L.Vector a

instance Element StructElem where
  getElementNodes  (StructElem nodes _)   = nodes
  getNumNodes      (StructElem nodes _)   = length nodes
  getConnectivity  (StructElem nodes _)   = map nodeNumber nodes
  getElementNumber (StructElem _ elemNum) = elemNum
  computeJacobian  (StructElem nodes elemNum) shpFcn coords = HMat.mul matA matB
    where
      dim  = getDimension shpFcn
      matA = L.tr $ L.fromLists $ map (dndXi shpFcn coords)
        [0..(getShapeFcnOrder shpFcn + 1)^dim - 1]
      matB = L.fromLists $ map nodeCoordinates nodes

  computeJacobianDet (StructElem nodes elemNum) shpFcn coords =
    L.det $ computeJacobian (StructElem nodes elemNum) shpFcn coords

  dndx elem shpFcn coords idx = HMat.app (HMat.inv (computeJacobian elem shpFcn coords))
    (L.fromList $ dndXi shpFcn coords idx)
