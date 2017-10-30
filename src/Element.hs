module Element
( Element
, StructElem(..)
, getElementNodes
, getConnectivity
, getElementNumber
, computeJacobian
, computeJacobianDet
) where

--import Numeric.HMatrix
import qualified Numeric.LinearAlgebra as NLinAl
import Numeric.LinearAlgebra (Matrix,fromLists,tr,det,(<>))

import Node
import Basis
import ShapeFcns

-- Element types
data StructElem a = StructElem [Node a] Int deriving (Show,Eq)
data Tri a = Tri [Node a] Int deriving (Show,Eq)

-- Only going to implement quads for now

class Element e where
  getElementNodes    :: (Fractional a) => e a -> [Node a]
  getConnectivity    :: (Fractional a) => e a -> [Int]
  getElementNumber   :: (Fractional a) => e a -> Int
  computeJacobian    :: (Basis b,ShapeFcn s,Fractional a,NLinAl.Element a,NLinAl.Numeric a) => e a -> s b -> [a] -> Matrix a
  computeJacobianDet :: (Basis b,ShapeFcn s,Fractional a,NLinAl.Element a,NLinAl.Numeric a,NLinAl.Field a) => e a -> s b -> [a] -> a

instance Element StructElem where
  getElementNodes (StructElem nodes _) = nodes
  getConnectivity (StructElem nodes _) = map nodeNumber nodes
  getElementNumber (StructElem _ elemNum) = elemNum
  computeJacobian (StructElem nodes elemNum) shpFcn coords = matA <> matB
    where
      dim = getDimension shpFcn
      matA = tr $ fromLists $ map (dndXi shpFcn coords) [0..(getShapeFcnOrder shpFcn + 1)^dim - 1]
      matB = fromLists $ map nodeCoordinates nodes
  computeJacobianDet (StructElem nodes elemNum) shpFcn coords = det $ computeJacobian (StructElem nodes elemNum) shpFcn coords
