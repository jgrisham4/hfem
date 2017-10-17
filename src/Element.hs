module Element
( Line(..)
, Quad(..)
, Tri(..)
) where

import Numeric.LinearAlgebra (fromLists,tr,det)

import Node
import ShapeFcns

-- Element types
data Line s a = Line s [Node a] Int deriving (Show,Eq)
data Quad s a = Quad s [Node a] Int deriving (Show,Eq)
data Tri  s a = Tri  s [Node a] Int deriving (Show,Eq)

-- Only going to implement quads for now

class Element e where
  getElementNumber   :: (ShapeFcn s)              => e s a -> Int
  getElementOrder    :: (ShapeFcn s)              => e s a -> Int
  computeJacobian    :: (ShapeFcn s,Fractional c) => e s a -> [c] -> Int -> c
  computeJacobianDet :: (ShapeFcn s,Fractional c) => e s a -> [c] -> Int -> c

instance Element Line where
  getElementNumber (Line _ _ elemNum) = elemNum
  getElementOrder  (Line shpFcn _ _) = getShapeFcnOrder shpFcn
  computeJacobian  (Line shpFcn nodes elemNum) coords = matA <> matB where
      matA = tr $ fromLists [map (dndXi shpFcn coords) [0..(getShapeFcnOrder shpFcn)]]
      matB = fromLists [map nodeCoordinates nodes]
  computeJacobianDet (Line shpFcn nodes elemNum) = det $ computeJacobian (Line shpFcn nodes elemNum)

--instance (Fractional a) => Element (Quad s a) where
--  getElementNumber (Quad _ _ elemNum) = elemNum
--  getElementOrder  (Quad shpFcn _ _) = getShapeFcnOrder shpFcn
--  computeJacobian  (Quad shpFcn nodes elemNum) coords = matA <> matB where
--      matA = tr $ fromLists [map (dndXi shpFcn coords) [0..(getShapeFcnOrder shpFcn + 1)^2]]
--      matB = fromLists [map nodeCoordinates nodes]
--  computeJacobianDet (Quad shpFcn nodes elemNum) = det $ computeJacobian (Quad shpFcn nodes elemNum)
