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
zeros :: (Num a,HMat.Element a) => Int -> HMat.Matrix a
zeros sz = HMat.fromLists [[0 | i <- [0..(sz-1)]] | j <- [0..(sz-1)]]

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
  --integrate          :: (Basis b,ShapeFcn s,Fractional a,L.Element a,L.Numeric a) => e a -> s b -> Int -> [Int] -> L.Matrix a

  -- Method for an inner product over an element
  -- What do I need to compute an inner product?  I need the shape functions to be integrated,
  -- I need the order of differentiation of each of the two terms.  I also need the number of
  -- Gauss points.
  innerProduct :: (Basis b,ShapeFcn s,Fractional a,L.Element a,L.Numeric a,L.Field a) => e a -> s b -> (Int,Int) -> Int -> L.Matrix a


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

  -- I need to figure out what gpt is.  Given some Gauss points, say [0,1], I need to figure out
  -- how to create a list which represents all combinations of these values.
  -- takes [0,1] and returns [[0,0],[0,1],[1,0],[1,1]]
  innerProduct (StructElem nodes elemNum) shpFcn deriv ngpts = foldr (+) (zeros nn) sampledMats
    where
      nn     = length nodes
      gdata  = getGaussPoints ngpts
      gpts1d = fst gdata
      gwts1d = snd gdata
      integrand coords = HMat.fromLists [[n shpFcn coords i (fst deriv) * n shpFcn coords j (snd deriv) | i <- [0..(nn-1)]] | j <- [0..(nn-1)]]
      gpts   = allCombinations gpts1d
      gwts   = allCombinations gwts1d
      sampledMats = zipWith (*) gwts (map integrand gpts)
