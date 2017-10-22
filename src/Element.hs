module Element
( Element(..)
) where
--( Line(..)
--, Quad(..)
--, Tri(..)
--) where

import Numeric.LinearAlgebra (fromLists,tr,det,(<>))

import Node
import Basis
import ShapeFcns

data Element s a = Element s [Node a] Int Int

getElementOrder :: (Element e b,ShapeFcn s,Fractional a) => e (s b) a -> Int
getElementOrder (Element shpFcn nodes elemNum dim) = getShapeFcnOrder shpFcn

{-
-- Element types
data Line s a = Line s [Node a] Int deriving (Show,Eq)
data Quad s a = Quad s [Node a] Int deriving (Show,Eq)
data Tri  s a = Tri  s [Node a] Int deriving (Show,Eq)

-- Only going to implement quads for now

class Element e where
  getElementNumber   :: (Fractional a) => e s a -> Int
  getElementOrder    :: (Fractional a) => e s a -> Int
  --computeJacobian    :: (ShapeFcn s,Fractional a) => e s a -> [a] -> Int -> a
  --computeJacobianDet :: (ShapeFcn s,Fractional a) => e s a -> [a] -> Int -> a

instance (ShapeFcn s,Fractional a) => Element Line where
  getElementNumber (Line _ _ elemNum) = elemNum
  getElementOrder  (Line shpFcn _ _) = getShapeFcnOrder shpFcn
  --computeJacobian  (Line shpFcn nodes elemNum) coords = matA <> matB where
  --    matA = tr $ fromLists [map (dndXi shpFcn coords) [0..(getShapeFcnOrder shpFcn)]]
  --    matB = fromLists [map nodeCoordinates nodes]
  --computeJacobianDet (Line shpFcn nodes elemNum) = det $ computeJacobian (Line shpFcn nodes elemNum)

--instance (Fractional a) => Element (Quad s a) where
--  getElementNumber (Quad _ _ elemNum) = elemNum
--  getElementOrder  (Quad shpFcn _ _) = getShapeFcnOrder shpFcn
--  computeJacobian  (Quad shpFcn nodes elemNum) coords = matA <> matB where
--      matA = tr $ fromLists [map (dndXi shpFcn coords) [0..(getShapeFcnOrder shpFcn + 1)^2]]
--      matB = fromLists [map nodeCoordinates nodes]
--  computeJacobianDet (Quad shpFcn nodes elemNum) = det $ computeJacobian (Quad shpFcn nodes elemNum)
-}
