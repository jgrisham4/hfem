module Element
( Line(..)
, Quad(..)
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
data Line a = Line [Node a] Int deriving (Show,Eq)
data Quad a = Quad [Node a] Int deriving (Show,Eq)
data Tri  a = Tri  [Node a] Int deriving (Show,Eq)

-- Only going to implement quads for now

class Element e where
  getElementNumber   :: (Fractional a,NLinAl.Element a,NLinAl.Numeric a) => e a -> Int
  computeJacobian    :: (Basis b,ShapeFcn s,Fractional a,NLinAl.Element a,NLinAl.Numeric a) => e a -> s b -> [a] -> Matrix a
  computeJacobianDet :: (Basis b,ShapeFcn s,Fractional a,NLinAl.Element a,NLinAl.Numeric a,NLinAl.Field a) => e a -> s b -> [a] -> a

instance Element Line where
  getElementNumber (Line _ elemNum) = elemNum
  computeJacobian  (Line nodes elemNum) shpFcn coords = matA <> matB where
      matA = tr $ fromLists $ map (dndXi shpFcn coords) [0..(getShapeFcnOrder shpFcn)]
      matB = fromLists $ map nodeCoordinates nodes
  computeJacobianDet (Line nodes elemNum) shpFcn coords = det $ computeJacobian (Line nodes elemNum) shpFcn coords

instance Element Quad where
  getElementNumber (Quad _ elemNum) = elemNum
  computeJacobian (Quad nodes elemNum) shpFcn coords = matA <> matB
    where
      dim = getDimension shpFcn
      matA = tr $ fromLists $ map (dndXi shpFcn coords) [0..(getShapeFcnOrder shpFcn + 1)^dim - 1]
      matB = fromLists $ map nodeCoordinates nodes
  computeJacobianDet (Quad nodes elemNum) shpFcn coords = det $ computeJacobian (Quad nodes elemNum) shpFcn coords
